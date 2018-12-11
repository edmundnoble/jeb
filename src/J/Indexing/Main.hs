{-# language NoMonomorphismRestriction #-}
{-# language DeriveFunctor #-}
{-# language DerivingVia #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
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
import Data.List(isSuffixOf)
import Data.Maybe(fromJust)
import System.FilePath((</>))
import System.Posix.Files(createSymbolicLink)

import qualified System.Directory as Dir
import qualified Data.Validation as Validation

import qualified J.Indexing.Types as Types
import qualified J.Indexing.Streengs as Streengs

ignoreSyncErrors :: IO () -> IO ()
ignoreSyncErrors io = void (try io :: IO (Either SomeException ()))

isValidDocFileName :: FilePath -> Bool
isValidDocFileName fp =
        ".md" `isSuffixOf` fp

type role Linker representational
newtype Linker a = Linker (String -> [Types.PrefixedTag] -> a)
        deriving (Functor, Applicative, Monad)
                via (ReaderT String ((->) [Types.PrefixedTag]))

link :: Linker a -> String -> [Types.PrefixedTag] -> a
link = coerce

fsLinker :: FilePath -> FilePath -> Linker (IO ())
fsLinker tagsFolder docsFolder = Linker linkDoc
        where
        linkDoc name = traverse_ (linkTag name)
        linkTag name (Types.PrefixedTag tag) = do
                absoluteTagFolder <-
                        Dir.makeAbsolute $ foldl' (</>) tagsFolder (reverse tag)
                Dir.createDirectoryIfMissing True absoluteTagFolder
                docFile <- Dir.makeAbsolute $ docsFolder </> name
                let symLinkPath = absoluteTagFolder </> name
                ignoreSyncErrors $ createSymbolicLink docFile symLinkPath

linkDocuments ::
        Linker (IO ()) ->
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
        let linkOrPrintErrs n =
                bimap (printErrs (Just n) . toList) (link linker n)
        let tags = (fmap . fmap) getPrefixedTags namedDocs
        Validation.codiagonal $ foldMap (uncurry linkOrPrintErrs) tags

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
                printDented = putStrLn . ("  " ++) . show
                printDocumentErr = printDented
                printFindingTagErr = printDented
                printWritingTagLinkErr = printDented

refreshIndex :: FilePath -> IO Bool
refreshIndex (dropWhile (== ' ') -> journalRoot) = do
        tagsFolder <- inRoot "tags"
        docsFolder <- inRoot "docs"
        tagMapFile <- inRoot "tagmap"
        let linker = fsLinker tagsFolder docsFolder
        let journalChecks =
                [
                Dir.doesDirectoryExist tagsFolder >>= printErr ("Tags folder " ++ tagsFolder ++ " doesn't exist!")
                ,       Dir.doesDirectoryExist docsFolder >>= printErr ("Docs folder " ++ docsFolder ++ " doesn't exist!")
                ,       Dir.doesFileExist tagMapFile >>= printErr ("Tag map file " ++ tagMapFile ++ " doesn't exist!")
                ]
        checkResults <- sequence journalChecks
        let sufficientDirectoryTree = and checkResults
        when sufficientDirectoryTree $
                linkDocuments linker tagsFolder docsFolder tagMapFile
        return sufficientDirectoryTree
        where
        inRoot = Dir.makeAbsolute . (journalRoot </>)
        printErr err b = unless b (putStrLn err) $> b
