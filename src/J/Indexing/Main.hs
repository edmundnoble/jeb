{-# language NoMonomorphismRestriction #-}

module J.Indexing.Main(refreshIndex) where

import Prelude hiding ((.), id)
import Control.Category(Category(..))
import Control.DeepSeq(force)
import Control.Exception(SomeException, evaluate, try)
import Control.Monad(filterM, unless)
import Data.Foldable(traverse_)
import Data.Functor(void)
import Data.List(isSuffixOf)
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
isValidDocFileName fp =
  (".md" `isSuffixOf` fp &&) <$> doesFileExist fp

linkTag :: FilePath -> FilePath -> String -> PrefixedTag -> IO ()
linkTag tagsFile docFile name (PrefixedTag tag) = do
  absoluteTagFile <- makeAbsolute $ foldl (</>) tagsFile (reverse tag)
  let makeDirs = createDirectoryIfMissing True absoluteTagFile
  let makeSymLink = (catchSyncErrors . createSymbolicLink docFile) (absoluteTagFile </> name)
  makeDirs *> makeSymLink

linkDocument ::
  FilePath ->
  FilePath ->
  DocumentMetadata String [PrefixedTag] ->
  IO ()
linkDocument tagsFile docsFile (DocumentMetadata name ts) = do
  docFile <- makeAbsolute $ docsFile </> name
  traverse_ (linkTag tagsFile docFile name) ts

linkDocuments :: FilePath -> FilePath -> FilePath -> IO ()
linkDocuments tagsFile docsFile tagMapFile = do
  tagMap <- readBulletedTagMap . lines <$> readFile tagMapFile
  _ <- evaluate (force tagMap)
  docFiles <- listDirectory docsFile >>= filterM isValidDocFileName
  let readDoc n = fmap (DocumentMetadata n) (readFile (docsFile </> n))
  let getPrefixedTags t = readPrefixedTags t tagMap

  namedDocs <- traverse readDoc docFiles
  _ <- evaluate (force namedDocs)

  catchSyncErrors (removeDirectoryRecursive tagsFile)
  createDirectoryIfMissing True tagsFile
  createDirectoryIfMissing True docsFile

  let linkDocNamed n =
        linkDocument tagsFile docsFile . DocumentMetadata n

  let linkOrPrintErrs (DocumentMetadata n v) =
        validation (printErrs (Just n) . NonEmpty.toList) (linkDocNamed n) v

  traverse_ linkOrPrintErrs ((fmap . fmap) getPrefixedTags namedDocs)

printErrs :: Maybe String -> [AnyErrors] -> IO ()
printErrs n es = do
  let
    AllErrors readingTagMapErrs readingDocumentErrs findingTagsErrs writingTagLinksErrs =
      collectErrors es
  unless (null readingTagMapErrs) $ do
    putStrLn "Errors reading tag map:"
    traverse_ printTagMapErr readingTagMapErrs
  unless (null readingDocumentErrs) $ do
    putStrLn (
      "Errors reading document (" ++
      fromJust n ++
      "):")
    traverse_ printDocumentErr readingDocumentErrs
  unless (null findingTagsErrs) $ do
    putStrLn (
      "Errors finding tags in tagmap (" ++
      fromJust n ++
      "):")
    traverse_ printFindingTagErr findingTagsErrs
  unless (null writingTagLinksErrs) $ do
    putStrLn "Errors writing symbolic links for tags:"
    traverse_ printWritingTagLinkErr writingTagLinksErrs
  where
    printTagMapErr a = case a of
      InvalidIndentation (Position l) minIndent indent ->
        putStrLn $
          "  At line " ++
          show l ++
          ", with minimum indentation at " ++
          show minIndent ++
          ", the indentation was " ++
          show indent ++
          " which is not divisible by the minimum." ++
          "  I don't know what level of nesting that is."
    printDented = putStrLn . ("  " ++) . show
    printDocumentErr = printDented
    printFindingTagErr = printDented
    printWritingTagLinkErr = printDented

refreshIndex :: FilePath -> IO ()
refreshIndex journalRoot =
  let inRoot = (</>) journalRoot in
  linkDocuments (inRoot "tags") (inRoot "docs") (inRoot "tagmap")
