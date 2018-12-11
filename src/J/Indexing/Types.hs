{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}

module J.Indexing.Types where

import Control.Lens(foldMapOf, makeClassyPrisms, makeLenses, makePrisms, over, traverseOf)
import Control.Monad(join)

import Data.Map(Map)

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

newtype PrefixedTag = PrefixedTag [String]
        deriving Show

data ErrorReadingDocument =
        NoTagSectionFound
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
