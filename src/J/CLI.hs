{-# language ApplicativeDo #-}

module J.CLI(opts) where

import Control.Applicative((<|>))
import Data.Semigroup((<>))
import qualified Options.Applicative as Options

import qualified J.Indexing.Main as Indexing

refresh :: Options.Parser (IO ())
refresh = Options.flag' Indexing.refreshIndex
  (  Options.long "refresh"
  <> Options.help "Refresh doc tags"
  )

logPath :: Options.Parser FilePath
logPath =
  Options.strOption $ Options.long "cyclelog" <> Options.value "cyclelog"

width :: Options.Parser Int
width = Options.option Options.auto (Options.long "width" <> Options.value 7)

-- preAuto :: Read a => (String -> String) -> Options.ReadM a
-- preAuto f = Options.eitherReader $ \arg -> case reads (f arg) of
--     [(r, "")] -> return r
--     _         -> Left $ "cannot parse value `" ++ arg ++ "'"

cli :: Options.Parser (IO ())
cli = refresh

opts :: Options.ParserInfo (IO ())
opts = Options.info (cli Options.<**> Options.helper)
  (  Options.fullDesc
  <> Options.progDesc "J tool" )
