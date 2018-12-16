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
import Control.Monad.Reader(Reader, ReaderT(..))
import Data.Coerce(coerce)
import Data.Functor.Identity(Identity(..))
import Data.Map.Strict(Map)
import Data.List(foldl', sort)

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.Map.Strict as Map

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

-- the derived ord instance does exactly what we want, *plus* working on
-- noncanonical (duplicate folder names) filesystems.
data TagFS = TagDir !String ![TagFS] | DocFile !String deriving (Eq, Ord)

sortFS :: TagFS -> TagFS
sortFS (TagDir n cs) = TagDir n (sortFS <$> (sort cs))
sortFS fs = fs

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
--         insertFS "doc1" (PrefixedTag ["tag2", "tag1"]) [TagDir "tag1" [TagDir "tag2" [DocFile "doc2"]]]
-- :}
-- tag1
--     tag2
--         doc1
--         doc2
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

-- | Computes the difference between two tag filesystems.
-- | Has colored output.
-- | Sort your filesystems before passing them in, pretty-please.
-- | This doesn't use any particularly nice diff algorithms, unfortunately.
-- | I plan to use patience diffing later.
-- Examples:
-- >>> :{
-- PP.plain $ diffTagFS (DocFile "hey") (DocFile "hello")
-- :}
-- - hey
-- + hello
--
-- >>> :{
-- PP.plain $ diffTagFS (DocFile "hey") (DocFile "hey")
-- :}
-- hey
--
-- >>> :{
-- PP.plain $ diffTagFS (TagDir "tag" [DocFile "hey"]) (TagDir "tag" [DocFile "hello"])
-- :}
-- tag
--     - hey
--     + hello
--
-- >>> :{
-- PP.plain $ diffTagFS (TagDir "tag1" [DocFile "hey"]) (TagDir "tag2" [DocFile "hello"])
-- :}
-- - tag1
--     hey
-- + tag2
--     hello
diffTagFS :: TagFS -> TagFS -> PP.Doc
diffTagFS (DocFile n) (DocFile n')
        | n == n' = PP.text n
        | otherwise =
                PP.red (PP.text ("- " ++ n)) PP.<$$>
                PP.green (PP.text ("+ " ++ n'))
diffTagFS fs@(TagDir n cs) fs'@(TagDir n' cs')
        | n == n'       = PP.nest 4 $ PP.vcat (PP.text n : (uncurry diffTagFS <$> zip cs cs'))
        | otherwise     = PP.red (PP.text "-" PP.<+> PP.pretty fs) PP.<$$>
                          PP.green (PP.text "+" PP.<+> PP.pretty fs')

-- | Create a tag filesystem from a single docuemnt with a single prefixed tag.
-- Examples:
-- >>> :{
-- PP.pretty $ singletonTagFS
--         "doc" (PrefixedTag ["taginner", "tagmiddle", "tagouter"])
-- :}
-- tagouter
--     tagmiddle
--         taginner
--             doc
singletonTagFS :: String -> PrefixedTag -> TagFS
singletonTagFS n (PrefixedTag tt) =
        foldl' (\a t -> TagDir t [a]) (DocFile n) tt

type role Linker representational
newtype Linker a = Linker (TagFS -> a)

link :: Linker a -> TagFS -> a
link = coerce
{-# inline conlike link #-}

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

type TagMap = Map String [String]
