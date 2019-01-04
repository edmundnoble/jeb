{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language RoleAnnotations #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}

module Jeb.Indexing.Types(
        TagMap, PrefixedTag(..), UnprefixedTag(..)
        ,TagFS(..)
        ,docMapToTagFS, tagFSToDocMap
        ,diffMultipleTagFS
        ,sortTagFS, sortTagFSS
        ,Located(..), Position(..)
        ,Linker(..), link
        ,showCouldntFindTagPrefix, showEmptyDocument, showEmptyTagSection
        ,showInvalidIndentation, showMultipleTagSections, showNoTagSectionFound
        ,showTagsFailedToParse, showUnresolvedTag
        ,anywhere, location
        ) where

import Control.Lens(foldMapOf, makeLenses, over, traverseOf)
import Control.Monad(join)
import Data.Algorithm.Diff(Diff(..), getDiffBy)
import Data.Coerce(coerce)
import Data.Function(on)
import Data.Map.Strict(Map)
import Data.List(foldl', sortOn)
import Data.Maybe(catMaybes)

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.Map.Strict as Map

type TagMap = Map String [String]

newtype Position = Position Int
        deriving Show

data Located a = Located {
        _location :: !Position,
        _anywhere :: a
} deriving Show

makeLenses ''Located

instance Functor Located where
        fmap = over anywhere

instance Foldable Located where
        foldMap = foldMapOf anywhere

instance Traversable Located where
        traverse = traverseOf anywhere

newtype UnprefixedTag = UnprefixedTag String
        deriving Show

-- by convention the actual tag is on the start, with the outermost
-- tag on the end.
newtype PrefixedTag = PrefixedTag [String]
        deriving Show

data TagFS = TagDir !String ![TagFS] | DocFile !String deriving (Eq, Ord)

fsName :: TagFS -> String
fsName (DocFile n) = n
fsName (TagDir n _) = n

sortTagFSS :: [TagFS] -> [TagFS]
sortTagFSS xs = sortOn fsName (sortTagFS <$> xs)

sortTagFS :: TagFS -> TagFS
sortTagFS (DocFile n) = DocFile n
sortTagFS (TagDir n ds) = TagDir n (sortTagFSS ds)

data Tree a = Tree a [Tree a]

-- | Converts a TagFS to a rose tree, not
-- | distinguishing between documents and empty folders.
tagFSToTree :: TagFS -> Tree String
tagFSToTree (TagDir n cs) = Tree n (tagFSToTree <$> cs)
tagFSToTree (DocFile n) = Tree n []

label :: Tree a -> a
label (Tree a _) = a

data DiffPrinter a
        = DiffPrinter {
                _printLabel :: a -> PP.Doc,
                _printFirst :: Tree a -> PP.Doc,
                _printSecond :: Tree a -> PP.Doc
        }

standardPrinter :: PP.Pretty a => (Tree a -> PP.Doc) -> DiffPrinter a
standardPrinter f =
        DiffPrinter
                PP.pretty
                (PP.red . (PP.text "-" PP.<+>) . f)
                (PP.green . (PP.text "+" PP.<+>) . f)

-- | General tree diffing algorithm.
-- | Given any ordinary diff algorithm and a way to print the labels of a rose
-- | tree, prints a diff of two lists of rose trees.
-- | If there is no difference, returns `Nothing`.
-- | Turns out, this is mostly the same thing tree-diff provides.
diffT ::
        Eq a =>
        (forall x. (x -> x -> Bool) -> [x] -> [x] -> [Diff x]) ->
        DiffPrinter a ->
        [Tree a] ->
        [Tree a] ->
        Maybe PP.Doc
diffT diffBy printer = go where
        go [] [] = Nothing
        go ts ts' = let
                diff = diffBy ((==) `on` label) ts ts'
                recurse (Both (Tree b cs) (Tree _ cs')) =
                        PP.nest 4 . (_printLabel printer b PP.<$$>) <$> go cs cs'
                recurse (First fs) =
                        Just $ _printFirst printer fs
                recurse (Second fs') =
                        Just $ _printSecond printer fs'
                loop = catMaybes $ recurse <$> diff in
                if null loop then Nothing else Just (PP.vcat loop)

-- | Prints a rose tree in the style I want filesystems printed, similarly
-- | to tree(1).
-- Examples:
-- >>> prettyFS (Tree "doc" [])
-- doc
-- >>> prettyFS (Tree "folder" [])
-- folder
-- >>> prettyFS (Tree "folder" [Tree "doc" []])
-- folder
--     doc
-- >>> prettyFS (Tree "folder1" [Tree "doc1" [], Tree "doc2" [], Tree "folder11" [Tree "doc3" []], Tree "doc4" []])
-- folder1
--     doc1
--     doc2
--     folder11
--         doc3
--     doc4

prettyFS :: PP.Pretty a => Tree a -> PP.Doc
prettyFS (Tree n ms) =
        PP.nest 4 $ PP.vcat (PP.pretty n : (prettyFS <$> ms))

instance PP.Pretty TagFS where
        pretty = prettyFS . tagFSToTree
        prettyList = PP.vcat . fmap PP.pretty

-- | Insert a tagged document into a tag filesystem.
-- | Note that this *must* operate on a *list* of `TagFS`,
-- | because otherwise there's no way to adjoin new tag trees at the root.
-- Examples:
-- >>> :{
-- PP.pretty $
--         insertFS "doc" (PrefixedTag ["tag1"]) []
-- :}
-- tag1
--     doc
--
-- >>> :{
-- PP.pretty $
--         insertFS "doc1" (PrefixedTag ["tag1"]) [TagDir "tag2" [DocFile "doc2"]]
-- :}
-- tag1
--     doc1
-- tag2
--     doc2
--
-- >>> :{
-- PP.pretty $
--         insertFS "doc1" (PrefixedTag ["tag3", "tag2", "tag1"])
--                 [TagDir "tag1" [TagDir "tag2" [TagDir "tag3" [DocFile "doc2"]]]]
-- :}
-- tag1
--     tag2
--         tag3
--             doc1
--             doc2
--

insertFS :: String -> PrefixedTag -> [TagFS] -> [TagFS]
insertFS n (PrefixedTag tt) = go (reverse tt)
        where
        tryInsert p t (TagDir sd cs) | sd == p = Just (TagDir sd (go t cs))
        tryInsert _ _ _ = Nothing

        replace _ [] = Nothing
        replace f ((f -> Just x):xs) = Just (x:xs)
        replace f (x:xs) = (x:) <$> replace f xs

        go (p:t) (replace (tryInsert p t) -> Just xs) = xs
        go ss xs = singletonTagFS n (PrefixedTag ss) : xs

-- | Computes the difference between two tag filesystems, if there is one.
-- | Has colored output.
-- | You may want to sort the output.
-- Examples:
-- >>> PP.plain . PP.pretty $ diffMultipleTagFS [DocFile "hey"] [DocFile "hello"]
-- - hey
-- + hello
--
-- >>> :{
-- PP.plain . PP.pretty $ diffMultipleTagFS [TagDir "tag" [DocFile "hey"]] [DocFile "hey"]
-- :}
-- - tag
--     hey
-- + hey
--
-- >>> :{
-- PP.plain . PP.pretty $ (diffMultipleTagFS
--           [TagDir "tag" [
--                    DocFile "hey"]]
--           [TagDir "tag" [
--                    DocFile "hello"]])
-- :}
-- tag
--     - hey
--     + hello
--
-- >>> :{
-- PP.plain $ PP.pretty (diffMultipleTagFS
--          [TagDir "tag1" [
--                   TagDir "tag11" [
--                            DocFile "hey"]]]
--          [TagDir "tag1" [
--                   TagDir "tag12" [
--                            DocFile "hey"]]])
-- :}
-- tag1
--     - tag11
--         hey
--     + tag12
--         hey
diffMultipleTagFS :: [TagFS] -> [TagFS] -> Maybe PP.Doc
diffMultipleTagFS fs fs' =
        diffT getDiffBy (standardPrinter prettyFS) (tagFSToTree <$> fs) (tagFSToTree <$> fs')

-- | Create a tag filesystem from a single document with a single prefixed tag.
-- Examples:
-- >>> :{
-- PP.pretty $ singletonTagFS
--         "doc" (PrefixedTag ["tagouter", "tagmiddle", "taginner"])
-- :}
-- tagouter
--     tagmiddle
--         taginner
--             doc

singletonTagFS :: String -> PrefixedTag -> TagFS
singletonTagFS n (PrefixedTag tt) =
        foldl' (\a t -> TagDir t [a]) (DocFile n) (reverse tt)

type role Linker representational
newtype Linker a = Linker ([TagFS] -> a)

link :: Linker a -> [TagFS] -> a
link = coerce
{-# inline conlike link #-}

-- | Converts a tag filesystem into a listing of its contents recursively,
-- | pairing filenames to full tag paths.

tagFSToDocMap :: TagFS -> [(String, [PrefixedTag])]
tagFSToDocMap = Map.toAscList . go []
        where
        go :: [String] -> TagFS -> Map String [PrefixedTag]
        go ss (DocFile n) = Map.singleton n [PrefixedTag ss]
        go ss (TagDir n cs) =
                Map.unionsWith (++) $ go (n:ss) <$> cs

docMapToTagFS :: [(String, [PrefixedTag])] -> [TagFS]
docMapToTagFS = foldl' (\b (n, ts) -> foldl' (flip (insertFS n)) b ts) []

makeIndent :: Int -> String
makeIndent n = join $ replicate n "  "

indentLines :: Int -> String -> String
indentLines n = unlines . fmap (makeIndent n ++) . lines

showNoTagSectionFound :: String
showNoTagSectionFound = "NoTagSectionFound"

showEmptyTagSection :: Position -> String
showEmptyTagSection (Position line) = "EmptyTagSection at line " ++ show line

showTagsFailedToParse :: Located String -> String
showTagsFailedToParse t =
        "TagsFailedToParse on line " ++ (show . _location) t ++ ":\n" ++
        indentLines 1 (show (_anywhere t))

showMultipleTagSections :: [Position] -> String
showMultipleTagSections xs =
        "MultipleTagSections on lines " ++ show xs

showEmptyDocument :: String
showEmptyDocument = "EmptyDocument"

showInvalidIndentation :: Position -> Int -> Int -> String
showInvalidIndentation (Position p) minIndent indent =
        "  At line " ++
        show p ++
        ", with minimum indentation at " ++
        show minIndent ++
        ", the indentation was " ++
        show indent ++
        " which is not divisible by the minimum." ++
        "  I don't know what level of nesting that is."

showCouldntFindTagPrefix :: UnprefixedTag -> String
showCouldntFindTagPrefix u =
        "Couldn't find tag prefix " ++ show u

showUnresolvedTag :: UnprefixedTag -> String
showUnresolvedTag s =
        "Unresolved tag " ++ show s
