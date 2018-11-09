{-# language NoMonomorphismRestriction #-}

module J.Indexing.Main(refreshIndex) where

import Prelude hiding ((.), id)
import Control.Category(Category(..))
import Control.DeepSeq
import Control.Exception(SomeException, evaluate, try)
import Control.Monad(filterM, unless)
import Data.Foldable(traverse_)
import Data.Functor(void)
import Data.List(isSuffixOf)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe(fromJust)
import Data.Validation(validation)
import System.Directory(
  createDirectoryIfMissing, doesFileExist, listDirectory,
  makeAbsolute, removeDirectoryRecursive)
import System.FilePath((</>))
import System.Posix.Files(createSymbolicLink)

import qualified Data.List.NonEmpty as NonEmpty

import J.Indexing.Types
import J.Indexing.Streengs

catchSyncErrors :: IO () -> IO ()
catchSyncErrors io =
  void (try io :: IO (Either SomeException ()))

isValidDocFileName :: FilePath -> IO Bool
isValidDocFileName d =
  (||) <$> pure (isSuffixOf ".md" d) <*> doesFileExist d

linkTag :: FilePath -> FilePath -> String -> PrefixedTag -> IO ()
linkTag tagsFile docFile name (PrefixedTag tag) = do
  absoluteTagFile <- makeAbsolute $ foldl (</>) tagsFile (reverse tag)
  let makeDirs = createDirectoryIfMissing True absoluteTagFile
  let makeSymLink = (catchSyncErrors . createSymbolicLink docFile) (absoluteTagFile </> name)
  makeDirs *> makeSymLink

linkTags :: FilePath -> FilePath -> String -> [PrefixedTag] -> IO ()
linkTags =
  ((traverse_ .) .) . linkTag

linkDocument :: FilePath -> FilePath -> (DocumentMetadata String [PrefixedTag]) -> IO ()
linkDocument tagsFile docsFile (DocumentMetadata name ts) = do
  docFile <- makeAbsolute $ docsFile </> name
  linkTags tagsFile docFile name ts

linkDocuments :: FilePath -> FilePath -> FilePath -> IO ()
linkDocuments tagsFile docsFile tagMapFile = do
  tagMap <- (readBulletedTagMap . lines) <$> readFile tagMapFile
  _ <- evaluate (force tagMap)
  docFiles <- listDirectory docsFile >>= filterM isValidDocFileName
  let readDoc n = fmap (DocumentMetadata n) (readFile (docsFile </> n))
  let getPrefixedTags t = readPrefixedTags t tagMap

  namedDocs <- traverse readDoc docFiles
  _ <- evaluate (force namedDocs)

  catchSyncErrors (removeDirectoryRecursive tagsFile)
  createDirectoryIfMissing True tagsFile
  createDirectoryIfMissing True docsFile

  let linkDocNamed n = linkDocument tagsFile docsFile . DocumentMetadata n

  let linkOrPrintErrs (DocumentMetadata n v) = validation (printErrs (Just n)) (linkDocNamed n) v

  traverse_ linkOrPrintErrs ((fmap . fmap) getPrefixedTags namedDocs)

printErrs :: Maybe String -> NonEmpty AnyErrors -> IO ()
printErrs n es = do
  let AllErrors errorsReadingTagMap errorsReadingDocument errorsFindingTags errorsWritingTagLinks = collectErrors (NonEmpty.toList es)
  unless (null errorsReadingTagMap) $ do
    putStrLn "Errors reading tag map:"
    traverse_ printTagMapError errorsReadingTagMap
  unless (null errorsReadingDocument) $ do
    putStrLn (
      "Errors reading document (" ++
      fromJust n ++
      "):")
    traverse_ printDocumentError errorsReadingDocument
  unless (null errorsFindingTags) $ do
    putStrLn (
      "Errors finding tags in tagmap (" ++
      fromJust n ++
      "):")
    traverse_ printFindingTagError errorsFindingTags
  unless (null errorsWritingTagLinks) $ do
    putStrLn "Errors writing symbolic links for tags:"
    traverse_ printWritingTagLinkError errorsWritingTagLinks
  where
    printTagMapError a = case a of
      InvalidIndentation (Position l) minIndent indent -> do
        putStrLn (
          "  At line " ++
          show l ++
          ", with minimum indentation at " ++
          (show minIndent) ++
          ", the indentation was " ++
          (show indent) ++
          " which is not divisible by the minimum." ++
          "  I don't know what level of nesting that is.")
    printDented = putStrLn . ((++) "  ") . show
    printDocumentError = printDented
    printFindingTagError = printDented
    printWritingTagLinkError = printDented

refreshIndex :: IO ()
refreshIndex =
  linkDocuments "tags" "docs" "tagmap"
