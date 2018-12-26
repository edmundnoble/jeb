{-# language ApplicativeDo #-}
{-# language NoMonomorphismRestriction #-}

module J.CLI(opts) where

import Data.Semigroup((<>))
import qualified Options.Applicative as O

import J.ErrT
import qualified J.Indexing.Main as Indexing

journalRoot :: O.Parser FilePath
journalRoot = O.strOption
        (  O.short 'r'
        <> O.long "journal-root"
        <> O.help "Should contain tagmap file, docs folder, and tags folder."
        <> O.value "."
        )

dryRun :: O.Parser Bool
dryRun = O.switch
        (  O.short 'd'
        <> O.long "dry-run"
        <> O.help "If true, only display a diff describing the desired changes"
        )

opts :: O.ParserInfo (ErrT IO ())
opts = O.info (cli O.<**> O.helper)
        (  O.fullDesc
        <> O.progDesc "J tool" )

cli :: O.Parser (ErrT IO ())
cli = Indexing.refreshIndex <$> journalRoot <*> (not <$> dryRun)
