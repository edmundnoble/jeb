{-# language NoMonomorphismRestriction #-}

module J.Indexing.Main(refreshIndex) where

import Prelude hiding ((.), id)
import Control.Category(Category(..))
import Control.DeepSeq(force)
import Control.Exception(SomeException, evaluate, try)
import Control.Monad(filterM, guard, unless)
import Data.Foldable(toList, traverse_)
import Data.Functor(($>), void)
import Data.List(isSuffixOf)
import Data.Maybe(fromJust)
import System.FilePath((</>))
import System.Posix.Files(createSymbolicLink)

import qualified System.Directory as Dir
import qualified Data.Validation as Validation

import J.Indexing.Types
import J.Indexing.Streengs

catchSyncErrors :: IO () -> IO ()
catchSyncErrors io =
  void (try io :: IO (Either SomeException ()))

isValidDocFileName :: FilePath -> IO Bool
isValidDocFileName fp =
  (".md" `isSuffixOf` fp &&) <$> Dir.doesFileExist fp

linkTag :: FilePath -> FilePath -> String -> PrefixedTag -> IO ()
linkTag tagsFile docFile name (PrefixedTag tag) = do
  absoluteTagFile <- Dir.makeAbsolute $ foldl (</>) tagsFile (reverse tag)
  let makeDirs = Dir.createDirectoryIfMissing True absoluteTagFile
  let makeSymLink = (catchSyncErrors . createSymbolicLink docFile) (absoluteTagFile </> name)
  makeDirs *> makeSymLink

linkDocument ::
  FilePath ->
  FilePath ->
  DocumentMetadata String [PrefixedTag] ->
  IO ()
linkDocument tagsFile docsFile (DocumentMetadata name ts) = do
  docFile <- Dir.makeAbsolute $ docsFile </> name
  traverse_ (linkTag tagsFile docFile name) ts

linkDocuments :: FilePath -> FilePath -> FilePath -> IO ()
linkDocuments tagsFile docsFile tagMapFile = do
  tagMap <- readBulletedTagMap . lines <$> readFile tagMapFile
  _ <- evaluate (force tagMap)
  docFiles <- Dir.listDirectory docsFile >>= filterM isValidDocFileName
  let readDoc n = fmap (DocumentMetadata n) (readFile (docsFile </> n))
  let getPrefixedTags t = readPrefixedTags t tagMap

  namedDocs <- traverse readDoc docFiles
  _ <- evaluate (force namedDocs)

  catchSyncErrors (Dir.removeDirectoryRecursive tagsFile)
  Dir.createDirectoryIfMissing True tagsFile
  Dir.createDirectoryIfMissing True docsFile

  let linkDocNamed n =
        linkDocument tagsFile docsFile . DocumentMetadata n

  let linkOrPrintErrs (DocumentMetadata n v) =
        Validation.validation (printErrs (Just n) . toList) (linkDocNamed n) v

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

refreshIndex :: FilePath -> IO Bool
refreshIndex journalRoot = do
  let inRoot = (</>) journalRoot
  let tagsFolder = inRoot "tags"
  let docsFolder = inRoot "docs"
  let tagMapFile = inRoot "tagmap"
  let orErr e = ($> e) . guard . not
  invalidJournalErr <-
    (orErr "Tags folder doesn't exist!" <$> Dir.doesDirectoryExist tagsFolder) <>
    (orErr "Docs folder doesn't exist!" <$> Dir.doesDirectoryExist docsFolder) <>
    (orErr "Tag map doesn't exist!"     <$> Dir.doesFileExist tagMapFile)
  case invalidJournalErr of
    [] ->
      linkDocuments tagsFolder docsFolder tagMapFile
    es ->
      traverse_ putStrLn es
  return $ null invalidJournalErr
