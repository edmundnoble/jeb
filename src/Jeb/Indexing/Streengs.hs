{-# language ViewPatterns #-}
{-# language TupleSections #-}

module Jeb.Indexing.Streengs(readBulletedTagMap,readPrefixedTags) where

import Prelude hiding((.), id)

import Control.Category(Category(..))
import Control.Monad(join)
import Control.Lens(foldOf)
import Data.Bifunctor(first)
import Data.Functor(($>))
import Data.List(isPrefixOf)
import Data.List.Split(splitWhen)
import Data.Maybe(fromMaybe)

import qualified Data.Map.Strict as Map

import Jeb.ErrT

import Jeb.Indexing.Types

-- | Does a string start with "Tags"?
--
-- Examples:
-- >>> (isTagHeader "Tags")
-- True
--
-- >>> (isTagHeader "Tags:")
-- True
--
-- >>> (isTagHeader "Tags: ")
-- True
--
-- >>> (isTagHeader "tags:")
-- False

isTagHeader :: String -> Bool
isTagHeader = ("Tags" `isPrefixOf`)

-- | Does a string start with "*"?
--
-- Examples:
-- >>> isBulleted "*Tag"
-- True
--
-- >>> isBulleted " *Tag"
-- False
--

isBulleted :: String -> Bool
isBulleted = ("*" `isPrefixOf`)

-- | Given the entire document, remove the `Tags` line and tell me where it is
--
-- Examples:
-- >>> :{
--   removeTagHeader [
--     Located (Position 0) ""
--   , Located (Position 1) "Tags"
--   , Located (Position 2) "* Tag 1"
--   , Located (Position 3) "* Tag 2"
--   ]
-- :}
-- Just (Located {_location = Position 1, _anywhere = [Located {_location = Position 2, _anywhere = "* Tag 1"},Located {_location = Position 3, _anywhere = "* Tag 2"}]})
--
-- >>> :{
--   removeTagHeader [Located (Position 0) "Hello world"]
-- :}
-- Nothing

removeTagHeader :: [Located String] -> Maybe (Located [Located String])
removeTagHeader = dropEmpties . takeAfter isTagHeader
        where
                dropEmpties = (fmap . fmap) $ dropWhile (all (== ' ') . _anywhere)

-- | Given the entire document, extract every line has a tag on it
--
-- Examples:
-- >>> :{
--   linesWithTags [
--     Located (Position 0) ""
--   , Located (Position 1) "Tags"
--   , Located (Position 2) "* Tag 1"
--   , Located (Position 3) "* Tag 2"
--   , Located (Position 4) ""
--   ]
-- :}
-- Just (Located {_location = Position 1, _anywhere = [Located {_location = Position 2, _anywhere = "* Tag 1"},Located {_location = Position 3, _anywhere = "* Tag 2"}]})
--
-- >>> :{
--   linesWithTags [Located (Position 0) "Hello world"]
-- :}
-- Nothing

linesWithTags :: [Located String] -> Maybe (Located [Located String])
linesWithTags ls =
        let trimmedWithNoTagHeader = removeTagHeader $ (fmap . fmap) trimStart ls in
                (fmap . fmap) (takeWhile (isBulleted . foldOf anywhere)) trimmedWithNoTagHeader

-- | Given a tagged document and tagmap, associates all of the document's tags
-- | with the path from each to the root of the tagmap.

readPrefixedTags ::
        String -> TagMap -> ErrT IO [PrefixedTag]
readPrefixedTags doc tagMap =
        join $ (fmap . traverse) prefixTags unPrefixedTags
        where
                unPrefixedTags = readUnprefixedTags (lines doc)
                prefixTags = flip getTagPrefixOrError tagMap

readUnprefixedTags ::
        [String] -> ErrT IO [UnprefixedTag]
readUnprefixedTags doc = let
        locatedDocLines = zipWithIndex doc
        tagLines = linesWithTags locatedDocLines
        parseTagOrReturn :: Located String -> Either (Located String) (Located [UnprefixedTag])
        parseTagOrReturn n = maybe (Left n) Right $ traverse parseTag n
        validTags :: Maybe (Located [Either (Located String) (Located [UnprefixedTag])])
        validTags = (fmap . fmap . fmap) parseTagOrReturn tagLines
        printParseError :: Located String -> ErrT IO ()
        printParseError = err . showTagsFailedToParse
        convertErrs :: Either (ErrT IO ()) [UnprefixedTag] -> ErrT IO [UnprefixedTag]
        convertErrs (Left fea) = fea $> []
        convertErrs (Right us) = pure us
        in case validTags of
                Nothing ->
                        err showNoTagSectionFound
                Just (Located p []) ->
                        err (showEmptyTagSection p)
                Just (Located _ xsm) ->
                        convertErrs $ ((_anywhere =<<) <$>
                                traverse (first printParseError) xsm)

-- | Trims the beginning of a string, removing whitespace.
-- | Only spaces are treated as whitespace deliberately.

trimStart :: String -> String
trimStart = dropWhile (== ' ')

takeAfter :: (a -> Bool) -> [Located a] -> Maybe (Located [Located a])
takeAfter _ [] =
        Nothing
takeAfter f (Located (Position p) (f -> True):xs) =
        Just (Located (Position p) xs)
takeAfter f (_:xs) =
        takeAfter f xs

zipWithIndex :: [a] -> [Located a]
zipWithIndex xs = uncurry Located <$> zip (Position <$> [0..]) xs

parseTag :: String -> Maybe [UnprefixedTag]
parseTag l =
        let trimmedLine = dropWhile (== ' ') l in
                if "*" `isPrefixOf` trimmedLine then
                        Just . fmap (UnprefixedTag . trimStart) $
                                splitWhen (== ',') (drop 1 trimmedLine)
                else
                        Nothing

-- Since the TagMap only contains scoping information,
-- we need to join the original tag onto the start.
getTagPrefix :: UnprefixedTag -> TagMap -> Maybe PrefixedTag
getTagPrefix (UnprefixedTag t) = fmap (PrefixedTag . (:) t) . Map.lookup t

getTagPrefixOrError ::
        UnprefixedTag -> TagMap -> ErrT IO PrefixedTag
getTagPrefixOrError t tagMap = case getTagPrefix t tagMap of
        Nothing -> err (showCouldntFindTagPrefix t)
        Just x -> pure x

minimumIndent :: [String] -> Maybe Int
minimumIndent =
        nonZero .
                foldr firstNonZero 0 .
                        computeIndentLengths
        where
                computeIndentLengths = fmap (length . takeWhile (== ' '))
                nonZero 0 = Nothing
                nonZero n = Just n
                firstNonZero a b = if a == 0 then b else a

-- | Reads a TagMap out of a list of lines.
--
-- Examples:
-- >>> import Data.List(sort)
-- >>> :{
-- (sort . fmap (\(k, v) -> k:v) . Map.toList . readBulletedTagMap) $
--      [
--      "Tags:",
--      "  * One tag",
--      "    * One tag in one tag",
--      "      * One tag in one tag in one tag",
--      "    * Two tag in one tag",
--      "      * One tag in two tag in one tag",
--      "  * Two tag"
--      ]
-- :}
-- [["One tag"],["One tag in one tag","One tag"],["One tag in one tag in one tag","One tag in one tag","One tag"],["One tag in two tag in one tag","Two tag in one tag","One tag"],["Two tag"],["Two tag in one tag","One tag"]]
--
-- >>> readBulletedTagMap []
-- fromList []

readBulletedTagMap :: [String] -> TagMap
readBulletedTagMap [] = mempty
readBulletedTagMap tagMapLines =
        loop [] $ indentForwardDifferences $ separateIndents <$> bulletLines
        where
        bulletLines = filter (isPrefixOf "*" . trimStart) tagMapLines

        minIndent = fromMaybe 2 $ minimumIndent bulletLines

        -- first, count the number of spaces in each indent,
        -- and extract the name of the tag.
        separateIndents :: String -> (Int, String)
        separateIndents l =
                let indent = (length . takeWhile (== ' ')) l in
                let tag = dropWhile (\c -> c == ' ' || c == '*') l in
                (indent, tag)

        -- then, calculate the forward differences of the sizes
        -- of the indents.
        indentForwardDifferences :: [(Int, a)] -> [(Int, a)]
        indentForwardDifferences = reverse . go 0 . reverse where
                go l ((x,y):xs) =
                        ((,y) $! (l-x)):(go x xs)
                go _ [] = []

        loop :: [String] -> [(Int, String)] -> TagMap
        loop ss ((x, y):xs) =
                let nextDelta = x `div` minIndent in
                let insertNew = Map.insert y ss in
                insertNew $
                        if nextDelta <= 0
                        then
                                loop (drop (-nextDelta) ss) xs
                        else
                                loop (y:ss) xs
        loop _ [] = Map.empty
