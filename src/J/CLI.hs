{-# language ApplicativeDo #-}

module J.CLI(opts) where

import Data.Semigroup((<>))
import qualified Options.Applicative as O

import qualified J.Indexing.Main as Indexing

refresh :: O.Parser (IO ())
refresh = O.flag' Indexing.refreshIndex
  (  O.long "refresh"
  <> O.help "Refresh doc tags"
  )

opts :: O.ParserInfo (IO ())
opts = O.info (cli O.<**> O.helper)
  (  O.fullDesc
  <> O.progDesc "J tool" )

cli :: O.Parser (IO ())
cli = refresh
