module J.Indexing.Streengs where

import Prelude hiding((.), id)

import Control.Category(Category(..))
import Control.Lens hiding ((<|))
import Data.Bifunctor(first)
import Data.List(isPrefixOf)
import Data.List.NonEmpty(NonEmpty(..))
import Data.List.Split(splitWhen)
import Data.Maybe(fromMaybe)
import Data.Validation(Validation(..))

import qualified Data.Map as Map
import qualified Data.Validation as Validation

import J.Indexing.Types

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
--
-- Examples:
-- >>> readPrefixedTags "" undefined :: Validation (NonEmpty AnyErrors) [PrefixedTag]
-- Failure (AnyErrorReadingDocument NoTagSectionFound :| [])
readPrefixedTags ::
        (AsErrorReadingDocument e, AsErrorFindingTag e) =>
        String -> TagMap -> Validation (NonEmpty e) [PrefixedTag]
readPrefixedTags doc tagMap =
        (Validation.fromEither . (=<<) Validation.toEither . Validation.toEither) $ prefixTags unPrefixedTags
        where
                unPrefixedTags = readUnprefixedTags (lines doc)
                prefixTags = (fmap . traverse) (first pure . flip getTagPrefixOrError tagMap)

readUnprefixedTags ::
        AsErrorReadingDocument e =>
        [String] -> Validation (NonEmpty e) [UnprefixedTag]
readUnprefixedTags doc =
        let locatedDocLines = zipWithIndex doc in
        let tagLines = linesWithTags locatedDocLines in
        let validTags = (fmap . fmap . fmap) parseTagValidated tagLines in
        case validTags of
                Nothing ->
                        Failure . pure $ review _NoTagSectionFound ()
                Just (Located p xsm) ->
                        case xsm of
                                [] -> Failure . pure $ review _EmptyTagSection p
                                _ -> (_anywhere =<<) <$>
                                        traverse (first (pure . review _TagsFailedToParse)) xsm

-- | Trims the beginning of a string, removing whitespace.
trimStart :: String -> String
trimStart = dropWhile (== ' ')

-- |
takeAfter :: (a -> Bool) -> [Located a] -> Maybe (Located [Located a])
takeAfter _ [] = Nothing
takeAfter f (Located (Position p) x:xs) | f x = Just (Located (Position p) xs)
takeAfter f (_:xs) = takeAfter f xs

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

parseTagValidated :: Located String -> Validation (Located String) (Located [UnprefixedTag])
parseTagValidated l@(Located p a) =
        maybe (Failure l) (Success . Located p) (parseTag a)

-- Since the TagMap only contains scoping information,
-- we need to join the original tag onto the end.
getTagPrefix :: UnprefixedTag -> TagMap -> Maybe PrefixedTag
getTagPrefix (UnprefixedTag t) = fmap (PrefixedTag . (:) t) . Map.lookup t

getTagPrefixOrError :: AsErrorFindingTag e =>
        UnprefixedTag -> TagMap -> Validation e PrefixedTag
getTagPrefixOrError t tagMap =
        maybe ((Failure . review _CouldntFindTagPrefix) t) Success (getTagPrefix t tagMap)

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

-- TODO: add Validation, proper error for unproportional indenting
readBulletedTagMap :: [String] -> TagMap
readBulletedTagMap [] = mempty
readBulletedTagMap tagMapLines = go minIndent [] bulletLines [] Map.empty
        where
                bulletLines = filter (isPrefixOf "*" . trimStart) tagMapLines

                minIndent = fromMaybe 2 $ minimumIndent bulletLines

                go :: Int -> [String] -> [String] -> String -> TagMap -> TagMap
                go lastIndent ss (l:ls) lastTag acc =
                        let indent = (length . takeWhile (== ' ')) l in
                        let tag = dropWhile ((||) <$> (== ' ') <*> (== '*')) l in
                        if (indent `mod` minIndent) /= 0 then
                        error "please use proportional indenting"
                        else if indent < lastIndent then
                        go indent (tail ss) ls tag (Map.insertWith (++) tag (tail ss) acc)
                        else if indent == lastIndent then
                        go indent ss ls tag (Map.insertWith (++) tag ss acc)
                        else
                        go indent (lastTag:ss) ls tag (Map.insertWith (++) tag (lastTag:ss) acc)
                go _ _ [] _ acc = acc
