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

module Jeb.Indexing.Main(refreshIndex) where

import Prelude hiding ((.), id)
import Control.Category(Category(..))
import Control.DeepSeq(force)
import Control.Exception(SomeException, evaluate, try)
import Control.Monad.Reader
import Data.Foldable(traverse_)
import Data.Functor(void)
import Data.List(isSuffixOf)
import System.FilePath((</>))
import System.Posix.Files(createSymbolicLink)
import Text.PrettyPrint.ANSI.Leijen(indent, putDoc, red)

import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified System.IO.Unsafe as U

import Jeb.Indexing.Types
import Jeb.Indexing.Streengs
import Jeb.ErrT

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
writeTagFS tagsPath docsPath (TagDir n fs) = do
        Dir.createDirectoryIfMissing False (tagsPath </> n)
        traverse_ (writeTagFS (tagsPath </> n) docsPath) fs
writeTagFS tagsPath docsPath (DocFile name) = do
        let symLinkPath = tagsPath </> name
        let docFile = docsPath </> name
        createSymbolicLink docFile symLinkPath

-- | Synchronize a TagFS from `docsFolder` onto a filesystem in `tagsFolder`,
fsLinker :: FilePath -> FilePath -> Linker (IO ())
fsLinker tagsFolder docsFolder = Linker fsLink
        where
        fsLink fss = do
                ignoreSyncErrors (Dir.removeDirectoryRecursive tagsFolder)
                Dir.createDirectoryIfMissing False tagsFolder
                traverse_ (writeTagFS tagsFolder docsFolder) fss

-- | Compute a diff from the filesystem in `tagsFolder`,
dryRunLinker :: FilePath -> Linker (IO ())
dryRunLinker tagsFolder = Linker dryRunLink
        where
        dryRunLink fss = do
                contents <- Dir.listDirectory tagsFolder
                oldFS <- traverse (readTagFS . (tagsFolder </>)) contents
                case diffMultipleTagFS oldFS fss of
                        Nothing -> putStrLn "Nothing to be done!"
                        Just p -> putDoc p *> putStrLn ""

-- | Given a linker in IO, create links for all of the documents in `docsFolder`,
-- | inside of `tagsFolder`, using `tagMapFile` as a reference for tag prefixes.
readDocMap ::
        FilePath ->
        FilePath ->
        ErrT IO [(String, [PrefixedTag])]
readDocMap docsFolder tagMapFile = do
        tagsE <- lift $ do
                tagMap <- readBulletedTagMap . lines <$> readFile tagMapFile
                _ <- evaluate (force tagMap)
                docContents <- Dir.listDirectory docsFolder
                let docNames = filter isValidDocFileName docContents
                let readDocNamed n = ((,) n) <$> readFile (docsFolder </> n)
                namedDocs <- traverse readDocNamed docNames
                _ <- evaluate (force namedDocs)
                return $ (fmap . fmap) (flip readPrefixedTags tagMap) namedDocs
        tags <- sequenceErrs $ sequenceA <$> tagsE
        return tags

refreshIndex :: FilePath -> Bool -> ErrT IO ()
refreshIndex (dropWhile (== ' ') -> journalRoot) reallyDoIt = do
        tagsFolder <- lift $ inJournalRoot "tags"
        docsFolder <- lift $ inJournalRoot "docs"
        tagMapFile <- lift $ inJournalRoot "tagmap"
        let synchronizeFilesystems = (if reallyDoIt
                then fsLinker
                else const . dryRunLinker) tagsFolder docsFolder
        void $ mapErr (red . indent 2) $ sequenceErrs
                [
                        Dir.doesDirectoryExist tagsFolder `orPrintErr`
                                ("Tags folder " ++ tagsFolder ++ " doesn't exist!")
                ,       Dir.doesDirectoryExist docsFolder `orPrintErr`
                                ("Docs folder " ++ docsFolder ++ " doesn't exist!")
                ,       Dir.doesFileExist tagMapFile `orPrintErr`
                                ("Tag map file " ++ tagMapFile ++ " doesn't exist!")
                ]
        docMap <- readDocMap docsFolder tagMapFile
        lift (link synchronizeFilesystems (docMapToTagFS docMap))
        where
        inJournalRoot = Dir.makeAbsolute . (journalRoot </>)
