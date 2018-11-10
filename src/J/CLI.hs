{-# language ApplicativeDo #-}

module J.CLI(opts) where

import Control.Applicative((<|>))
import Data.Semigroup((<>))
import qualified Options.Applicative as Options

import J.Cycles.Types

import qualified J.Cycles.Main as Cycles
import qualified J.Indexing.Main as Indexing

refresh :: Options.Parser (IO ())
refresh = Options.flag' Indexing.refreshIndex
  (  Options.long "refresh"
  <> Options.help "Refresh doc tags"
  )

view :: Options.Parser (ViewerConfig -> IO ())
view = Options.flag' Cycles.brickMain
  (  Options.long "view"
  <> Options.help "View cycle history"
  )

logPath :: Options.Parser FilePath
logPath =
  Options.strOption ((Options.long "cyclelog") <> Options.value "cyclelog")

-- day :: Options.Parser (Maybe Day)
-- day = Options.optional $ Options.option Options.auto
--   (  Options.long "day"
--   <> Options.help "Day to view (on the right)"
--   )

width :: Options.Parser Int
width = Options.option Options.auto (Options.long "width" <> Options.value 7)

-- preAuto :: Read a => (String -> String) -> Options.ReadM a
-- preAuto f = Options.eitherReader $ \arg -> case reads (f arg) of
--     [(r, "")] -> return r
--     _         -> Left $ "cannot parse value `" ++ arg ++ "'"

viewerConfig :: Options.Parser ViewerConfig
viewerConfig = do
  lp <- logPath
  w <- width
  return ViewerConfig {
    _configLogPath = lp
  , _configIntervalSize = w
  }

cli :: Options.Parser (IO ())
cli = (view <*> viewerConfig) <|> refresh

opts :: Options.ParserInfo (IO ())
opts = Options.info (cli Options.<**> Options.helper)
  (  Options.fullDesc
  <> Options.progDesc "J tool" )
