{-# language DeriveFunctor #-}
{-# language DerivingVia #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NoMonomorphismRestriction #-}
{-# language RoleAnnotations #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}

module J.Indexing.Main(refreshIndex) where

import Prelude hiding ((.), id)
import Control.Category(Category(..))
import Control.DeepSeq(force)
import Control.Exception(SomeException, evaluate, try)
import Control.Monad(unless)
import Control.Monad.Reader
import Data.Bifunctor(bimap)
import Data.Coerce
import Data.Foldable(foldl', toList, traverse_)
import Data.Functor(($>), void)
import Data.Functor.Identity(Identity(..))
import Data.List(intercalate, isSuffixOf)
import Data.Maybe(fromJust)
import Data.Map.Lazy(Map)
import System.FilePath((</>))
import System.Posix.Files(createSymbolicLink)

import qualified System.Directory as Dir
import qualified Data.Map.Lazy as Map
import qualified Data.Validation as Validation

import qualified J.Indexing.Types as Types
import qualified J.Indexing.Streengs as Streengs

ignoreSyncErrors :: IO () -> IO ()
ignoreSyncErrors io = void (try io :: IO (Either SomeException ()))

isValidDocFileName :: FilePath -> Bool
isValidDocFileName =
        (".md" `isSuffixOf`)

-- | Mirrors a TagFS onto a real filesystem in `tagsFolder`,
fsLinker :: FilePath -> FilePath -> Types.Linker (IO ())
fsLinker tagsFolder docsFolder = Types.Linker fsLink
        where
        fsLink fs = do
                ignoreSyncErrors (Dir.removeDirectoryRecursive tagsFolder)
                Dir.createDirectoryIfMissing False tagsFolder
                Streengs.writeTagFS tagsFolder docsFolder fs

-- | Given a linker in IO, create links for all of the documents in `docsFolder`,
-- | inside of `tagsFolder`, using `tagMapFile` as a reference for tag prefixes.
linkDocuments ::
        Types.Linker (IO ()) ->
        FilePath ->
        FilePath ->
        FilePath ->
        IO ()
linkDocuments linker tagsFolder docsFolder tagMapFile = do
        tagMap <- Streengs.readBulletedTagMap . lines <$> readFile tagMapFile
        _ <- evaluate (force tagMap)
        docContents <- Dir.listDirectory docsFolder
        let docNames = filter isValidDocFileName docContents
        let readDocNamed n = ((,) n) <$> readFile (docsFolder </> n)
        let getPrefixedTags = flip Streengs.readPrefixedTags tagMap
        namedDocs <- traverse readDocNamed docNames
        _ <- evaluate (force namedDocs)
        ignoreSyncErrors (Dir.removeDirectoryRecursive tagsFolder)
        Dir.createDirectoryIfMissing True tagsFolder
        -- let linkOrPrintErrs n = bimap (printErrs (Just n) . toList) (Types.link linker n)
        let tags = (fmap . fmap) getPrefixedTags namedDocs
        -- Validation.codiagonal $ foldMap (uncurry linkOrPrintErrs) tags
        undefined

printErrs :: Maybe String -> [Types.AnyErrors] -> IO ()
printErrs n es = do
        let Types.AllErrors errs1 errs2 errs3 errs4 =
                Types.collectErrors es
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
                        Types.InvalidIndentation (Types.Position l) minIndent indent ->
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

refreshIndex :: FilePath -> IO Bool
refreshIndex (dropWhile (== ' ') -> journalRoot) = do
        tagsFolder <- inJournalRoot "tags"
        docsFolder <- inJournalRoot "docs"
        tagMapFile <- inJournalRoot "tagmap"
        let linker = fsLinker tagsFolder docsFolder
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
                linkDocuments linker tagsFolder docsFolder tagMapFile
        return noMissingPaths
        where
        inJournalRoot = Dir.makeAbsolute . (journalRoot </>)
        orPrintErr q err = q >>= \b -> unless b (putStrLn err) $> b
