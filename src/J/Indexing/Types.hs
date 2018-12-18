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

module J.Indexing.Types where

import Control.Lens(foldMapOf, makeClassyPrisms, makeLenses, makePrisms, over, traverseOf)
import Control.Monad(join)
import Data.Algorithm.Diff(Diff(..), getDiffBy)
import Data.Coerce(coerce)
import Data.Function(on)
import Data.Map.Strict(Map)
import Data.List(foldl')
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

-- |
-- Examples:
-- >>> PP.pretty (DocFile "doc")
-- doc
-- >>> PP.pretty (TagDir "folder" [])
-- folder
-- >>> PP.pretty (TagDir "folder" [DocFile "doc"])
-- folder
--     doc
-- >>> let doc1 = ("folder1", DocFile "doc1")
-- >>> let doc2 = ("folder2", DocFile "doc2")
-- >>> let three = ("folder2", DocFile "doc2")
-- >>> PP.pretty (TagDir "folder1" [DocFile "doc1", DocFile "doc2", TagDir "folder11" [DocFile "doc3"], DocFile "doc4"])
-- folder1
--     doc1
--     doc2
--     folder11
--         doc3
--     doc4

instance PP.Pretty TagFS where
        pretty (DocFile n) = PP.text n
        pretty (TagDir n ms) =
                PP.nest 4 $ PP.vcat (PP.text n : (PP.pretty <$> ms))
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

fsName :: TagFS -> String
fsName (DocFile n) = n
fsName (TagDir n _) = n

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
diffMultipleTagFS [] [] = Nothing
diffMultipleTagFS fss fss' = let
        differentlyNamed = getDiffBy ((==) `on` fsName) fss fss'
        recurse (Both (TagDir n cs) (TagDir _ cs')) =
                PP.nest 4 . (PP.text n PP.<$$>) <$> diffMultipleTagFS cs cs'
        recurse (Both (DocFile n) (DocFile n')) | n == n' =
                Nothing
        recurse (Both fs fs') = Just (
                PP.red (PP.text "-" PP.<+> PP.pretty fs) PP.<$$>
                PP.green (PP.text "+" PP.<+> PP.pretty fs'))
        recurse (First fs) =
                Just $ PP.red (PP.text "-" PP.<+> PP.pretty fs)
        recurse (Second fs') =
                Just $ PP.green (PP.text "+" PP.<+> PP.pretty fs')
        loop = catMaybes $ recurse <$> differentlyNamed in
        if null loop
        then Nothing
        else Just (PP.vcat loop)


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

data ErrorReadingDocument
        = NoTagSectionFound
        | EmptyTagSection Position
        | TagsFailedToParse (Located String)
        | MultipleTagSections [Position]
        | EmptyDocument

makeIndent :: Int -> String
makeIndent n = join $ replicate n "  "

indentLines :: Int -> String -> String
indentLines n = unlines . fmap (makeIndent n ++) . lines

instance Show ErrorReadingDocument where
        show NoTagSectionFound = "NoTagSectionFound"
        show (EmptyTagSection (Position line)) = "EmptyTagSection at line " ++ show line
        show (TagsFailedToParse t) =
                "TagsFailedToParse on line " ++ (show . _location) t ++ ":\n" ++
                indentLines 1 (show (_anywhere t))
        show (MultipleTagSections xs) = "MultipleTagSections on lines " ++ show xs
        show EmptyDocument = "EmptyDocument"

makeClassyPrisms 'NoTagSectionFound

data ErrorReadingTagMap = InvalidIndentation Position Int Int
        deriving Show

makeClassyPrisms 'InvalidIndentation

newtype ErrorFindingTag = CouldntFindTagPrefix UnprefixedTag
        deriving Show

makeClassyPrisms 'CouldntFindTagPrefix

newtype ErrorWritingTagLinks = UnresolvedTag String
        deriving Show

makeClassyPrisms 'UnresolvedTag

-- invariant: must be at least one error in an AllErrors.
data AllErrors =
        AllErrors [ErrorReadingTagMap] [ErrorReadingDocument] [ErrorFindingTag] [ErrorWritingTagLinks]
        deriving (Show)

instance Semigroup AllErrors where
        (AllErrors ertms erds efts ewtls) <> (AllErrors ertms' erds' efts' ewtls') =
                AllErrors (ertms ++ ertms') (erds ++ erds') (efts ++ efts') (ewtls ++ ewtls')

instance Monoid AllErrors where
        mappend = (<>)
        mempty = AllErrors [] [] [] []

collectErrors :: [AnyErrors] -> AllErrors
collectErrors = go mempty where
        go (AllErrors ertms erds efts ewtls) (x:xs) = case x of
                AnyErrorReadingTagMap e -> go (AllErrors (e:ertms) erds efts ewtls) xs
                AnyErrorReadingDocument e -> go (AllErrors ertms (e:erds) efts ewtls) xs
                AnyErrorFindingTag e -> go (AllErrors ertms erds (e:efts) ewtls) xs
                AnyErrorWritingTagLinks e -> go (AllErrors ertms erds efts (e:ewtls)) xs
        go acc [] = acc

data AnyErrors
        = AnyErrorReadingDocument ErrorReadingDocument
        | AnyErrorReadingTagMap ErrorReadingTagMap
        | AnyErrorFindingTag ErrorFindingTag
        | AnyErrorWritingTagLinks ErrorWritingTagLinks
        deriving Show

makePrisms 'AnyErrorReadingDocument

instance AsErrorReadingDocument AnyErrors where
        _ErrorReadingDocument = _AnyErrorReadingDocument

instance AsErrorReadingTagMap AnyErrors where
        _ErrorReadingTagMap = _AnyErrorReadingTagMap

instance AsErrorFindingTag AnyErrors where
        _ErrorFindingTag = _AnyErrorFindingTag

instance AsErrorWritingTagLinks AnyErrors where
        _ErrorWritingTagLinks = _AnyErrorWritingTagLinks
