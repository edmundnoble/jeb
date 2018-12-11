{-# language ApplicativeDo #-}
{-# language NoMonomorphismRestriction #-}

module J.CLI(opts) where

import Data.Semigroup((<>))
import qualified Options.Applicative as O

import qualified J.Indexing.Main as Indexing

journalRoot :: O.Parser FilePath
journalRoot = O.strOption
        (  O.short 'r'
        <> O.long "journal-root"
        <> O.help "Should contain tagmap file, docs folder, and tags folder."
        <> O.value "."
        )

opts :: O.ParserInfo (IO Bool)
opts = O.info (cli O.<**> O.helper)
        (  O.fullDesc
        <> O.progDesc "J tool" )

cli :: O.Parser (IO Bool)
cli = Indexing.refreshIndex <$> journalRoot
