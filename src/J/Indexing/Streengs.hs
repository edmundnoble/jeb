module J.Indexing.Streengs(readBulletedTagMap, readPrefixedTags) where

import Prelude hiding((.), id)

import Control.Category(Category(..))
import Control.Lens hiding ((<|))
import Data.Char(isSpace)
import Data.List(isPrefixOf)
import Data.List.NonEmpty(NonEmpty(..))
import Data.List.Split(splitWhen)
import Data.Maybe(fromMaybe, listToMaybe)
import Data.Validation(Validation(..))

import qualified Data.Map as Map
import qualified Data.Validation as Validation

import J.Indexing.Types

isTagHeader :: String -> Bool
isTagHeader =
  (||) <$> ("tag" `isPrefixOf`) <*> ("Tag" `isPrefixOf`)

isBulletLine :: String -> Bool
isBulletLine = isPrefixOf "*"

{-# inline isEmptyLine #-}
isEmptyLine :: String -> Bool
isEmptyLine = all isSpace

removeTagHeader :: [Located String] -> Maybe (Located [Located String])
removeTagHeader = dropEmpties . takeAfter isTagHeader
  where
    dropEmpties = (fmap . fmap) $ dropWhile (isEmptyLine . _anywhere)

linesWithTags :: [Located String] -> Maybe (Located [Located String])
linesWithTags ls =
  let trimmedWithNoTagHeader = removeTagHeader $ (fmap . fmap) trimStart ls in
    (fmap . fmap) (takeWhile (isBulletLine . foldOf anywhere)) trimmedWithNoTagHeader

readPrefixedTags ::
  (AsErrorReadingDocument e, AsErrorFindingTag e) =>
  String -> TagMap -> Validation (NonEmpty e) [PrefixedTag]
readPrefixedTags doc tagMap =
  (Validation.fromEither . (=<<) Validation.toEither . Validation.toEither) $ prefixTags unPrefixedTags
  where
    unPrefixedTags = readUnprefixedTags (lines doc)
    prefixTags = (fmap . traverse) (leftMap singletonNE . flip getTagPrefixOrError tagMap)


readUnprefixedTags ::
  AsErrorReadingDocument e =>
  [String] -> Validation (NonEmpty e) [UnprefixedTag]
readUnprefixedTags doc =
  let locatedDocLines = zipWithIndex doc in
  let tagLines = linesWithTags locatedDocLines in
  let validTags = (fmap . fmap . fmap) parseTagValidated tagLines in
    case validTags of
      Nothing ->
        errNE $ review _NoTagSectionFound ()
      Just (Located p xsm) ->
        case xsm of
          [] -> errNE $ review _EmptyTagSection p
          _ -> (_anywhere =<<) <$>
            traverse (leftMap (singletonNE . review _TagsFailedToParse)) xsm

errNE :: e -> Validation (NonEmpty e) a
errNE = Failure . singletonNE

singletonNE :: e -> NonEmpty e
singletonNE = flip (:|) []

trimStart :: String -> String
trimStart s = dropWhile isSpace s

takeAfter :: (a -> Bool) -> [Located a] -> Maybe (Located [Located a])
takeAfter _ [] = Nothing
takeAfter f ((Located (Position p) x):xs) | f x = Just (Located (Position p) xs)
takeAfter f (_:xs) = takeAfter f xs

zipWithIndex :: [a] -> [Located a]
zipWithIndex xs = uncurry Located <$> (zip (Position <$> [0..]) xs)

parseTag :: String -> Maybe [UnprefixedTag]
parseTag l =
  let trimmedLine = dropWhile isSpace l in
    if isPrefixOf "*" trimmedLine then
      Just . fmap (UnprefixedTag . trimStart) $
        splitWhen ((==) ',') (drop 1 trimmedLine)
    else
      Nothing

parseTagValidated :: Located String -> Validation (Located String) (Located [UnprefixedTag])
parseTagValidated l@(Located p a) =
  maybe (Failure l) (Success . Located p) (parseTag a)

leftMap :: (e -> e') -> Validation e a -> Validation e' a
leftMap f (Failure e) = Failure (f e)
leftMap _ (Success a) = Success a

-- Since the TagMap only contains scoping information,
-- we need to join the original tag onto the end.
getTagPrefix :: UnprefixedTag -> TagMap -> Maybe PrefixedTag
getTagPrefix (UnprefixedTag t) = fmap (PrefixedTag . (:) t) . Map.lookup t

getTagPrefixOrError :: AsErrorFindingTag e =>
  UnprefixedTag -> TagMap -> Validation e PrefixedTag
getTagPrefixOrError t tagMap =
  maybe ((Failure . review _CouldntFindTagPrefix) t) Success (getTagPrefix t tagMap)

-- TODO: add Validation, proper error for unproportional indenting
readBulletedTagMap :: [String] -> TagMap
readBulletedTagMap tmLines = do
  let bulletLines = filter (isPrefixOf "*" . trimStart) tmLines

  let minIndent = fromMaybe 1 $ (length . takeWhile isSpace) <$> (listToMaybe bulletLines)

  go minIndent minIndent [] bulletLines [] Map.empty
    where
      go :: Int -> Int -> [String] -> [String] -> String -> TagMap -> TagMap
      go minIndent lastIndent ss (l:ls) lastTag m =
        let indent = (length . takeWhile isSpace) l in
          let tag = dropWhile ((||) <$> isSpace <*> ((==) '*')) l in
          if (indent `mod` minIndent) /= 0 then
            error "please use proportional indenting"
          else if indent < lastIndent then
            go minIndent indent (tail ss) ls tag $ Map.insertWith (++) tag (tail ss) m
          else if indent == lastIndent then
            go minIndent indent ss ls tag $ Map.insertWith (++) tag ss m
          else
            go minIndent indent (lastTag:ss) ls tag $ Map.insertWith (++) tag (lastTag:ss) m
      go _ _ _ [] _ m = m
