{-# language DeriveFunctor #-}
{-# language DerivingVia #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RoleAnnotations #-}
{-# language LambdaCase #-}
{-# language MonoLocalBinds #-}
{-# language NoMonomorphismRestriction #-}
{-# language ViewPatterns #-}

module J.Indexing.Main(refreshIndex) where

import Prelude hiding ((.), id)
import Control.Category(Category(..))
import Control.DeepSeq(force)
import Control.Exception(SomeException, evaluate, try)
import Control.Monad(unless)
import Control.Monad.Reader
import Data.Foldable(toList, traverse_)
import Data.Functor(($>), void)
import Data.List(isSuffixOf)
import Data.Maybe(fromJust)
import System.FilePath((</>))
import System.Posix.Files(createSymbolicLink)
import Text.PrettyPrint.ANSI.Leijen(putDoc)

import qualified Data.Validation as Validation
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified System.IO.Unsafe as U

import J.Indexing.Types
import J.Indexing.Streengs

ignoreSyncErrors :: IO () -> IO ()
ignoreSyncErrors io = void (try io :: IO (Either SomeException ()))

isValidDocFileName :: FilePath -> Bool
isValidDocFileName =
        (".md" `isSuffixOf`)

-- | Read a document filesystem from disk.
readTagFS :: FilePath -> IO TagFS
readTagFS path = do
        let baseName = FP.takeFileName path
        isDir <- Dir.doesDirectoryExist path
        if isDir
        then do
                contents <- Dir.listDirectory path
                let absolutize = (path </>)
                let readSubdirFS n =
                        U.unsafeInterleaveIO (readTagFS (absolutize n))
                recFss <- traverse readSubdirFS contents
                return $ TagDir baseName recFss
        else return $ DocFile baseName

-- | Write the document filesystem from disk.
writeTagFS :: FilePath -> FilePath -> TagFS -> IO ()
writeTagFS tagsPath docsPath (TagDir n fs) =
        traverse_ (writeTagFS (tagsPath </> n) docsPath) fs
writeTagFS tagsPath docsPath (DocFile name) = do
        let symLinkPath = tagsPath </> name
        let docFile = docsPath </> name
        createSymbolicLink docFile symLinkPath

-- | Mirrors a TagFS onto a real filesystem in `tagsFolder`,
fsLinker :: FilePath -> FilePath -> Linker (IO ())
fsLinker tagsFolder docsFolder = Linker (traverse_ fsLink)
        where
        fsLink fs = do
                ignoreSyncErrors (Dir.removeDirectoryRecursive tagsFolder)
                Dir.createDirectoryIfMissing False tagsFolder
                writeTagFS tagsFolder docsFolder fs

dryRunLinker :: FilePath -> Linker (IO ())
dryRunLinker tagsFolder = Linker (dryRunLink)
        where
        dryRunLink fss = do
                contents <- Dir.listDirectory tagsFolder
                oldFS <- traverse (readTagFS . (tagsFolder </>)) contents
                let diff = diffMultipleTagFS oldFS fss
                case diff of
                        Nothing -> putStrLn "Nothing to be done!"
                        Just p -> putDoc p *> putStrLn ""

-- | Given a linker in IO, create links for all of the documents in `docsFolder`,
-- | inside of `tagsFolder`, using `tagMapFile` as a reference for tag prefixes.
linkDocuments ::
        Linker (IO ()) ->
        FilePath ->
        FilePath ->
        IO ()
linkDocuments linker docsFolder tagMapFile = do
        tagMap <- readBulletedTagMap . lines <$> readFile tagMapFile
        _ <- evaluate (force tagMap)
        docContents <- Dir.listDirectory docsFolder
        let docNames = filter isValidDocFileName docContents
        let readDocNamed n = ((,) n) <$> readFile (docsFolder </> n)
        namedDocs <- traverse readDocNamed docNames
        _ <- evaluate (force namedDocs)
        let tags = (fmap . fmap) (flip readPrefixedTags tagMap) namedDocs
        traverse_ (\(n, v) -> Validation.validation (printErrs (Just n) . toList) (const (pure ())) v) tags
        traverse_ (link linker . docMapToTagFS) (sequenceA (sequenceA <$> tags))

printErrs :: Maybe String -> [AnyErrors] -> IO ()
printErrs n es = do
        let AllErrors errs1 errs2 errs3 errs4 =
                collectErrors es
        unless (null errs1) $ do
                putStrLn "Errors reading tag map:"
                traverse_ printTagMapErr errs1
        unless (null errs2) $ do
                putStrLn (
                        "Errors reading document (" ++
                        fromJust n ++
                        "):")
                traverse_ printDocumentErr errs2
        unless (null errs3) $ do
                putStrLn (
                        "Errors finding tags in tagmap (" ++
                        fromJust n ++
                        "):")
                traverse_ printFindingTagErr errs3
        unless (null errs4) $ do
                putStrLn "Errors writing symbolic links for tags:"
                traverse_ printWritingTagLinkErr errs4
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
                printDented :: Show a => a -> IO ()
                printDented = putStrLn . ("  " ++) . show
                printDocumentErr = printDented
                printFindingTagErr = printDented
                printWritingTagLinkErr = printDented

refreshIndex :: FilePath -> Bool -> IO ()
refreshIndex (dropWhile (== ' ') -> journalRoot) reallyDoIt = do
        tagsFolder <- inJournalRoot "tags"
        docsFolder <- inJournalRoot "docs"
        tagMapFile <- inJournalRoot "tagmap"
        let linker = (if reallyDoIt
                then fsLinker
                else const . dryRunLinker) tagsFolder docsFolder
        noMissingPaths <- and <$> sequence
                [
                        Dir.doesDirectoryExist tagsFolder `orPrintErr`
                                ("Tags folder " ++ tagsFolder ++ " doesn't exist!")
                ,       Dir.doesDirectoryExist docsFolder `orPrintErr`
                                ("Docs folder " ++ docsFolder ++ " doesn't exist!")
                ,       Dir.doesFileExist tagMapFile `orPrintErr`
                                ("Tag map file " ++ tagMapFile ++ " doesn't exist!")
                ]
        when noMissingPaths $
                linkDocuments linker docsFolder tagMapFile
        where
        inJournalRoot = Dir.makeAbsolute . (journalRoot </>)
        orPrintErr q err = q >>= \b -> unless b (putStrLn err) $> b
