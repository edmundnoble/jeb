{-# language DoAndIfThenElse #-}

import System.Directory
import System.FilePath.Posix
import Test.DocTest

allHsFiles p = do
        isDir <- doesDirectoryExist p
        if isDir
        then do
                children <- listDirectory p
                let absoluteChildren = (fmap (p </>) children)
                concat <$> traverse allHsFiles absoluteChildren
        else if takeExtension p == ".hs"
        then pure [p]
        else pure []

main = allHsFiles "src" >>= doctest
