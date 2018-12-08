module Main where

import Prelude

import Control.Monad(join)
import Options.Applicative(ParserResult(..), execParser, execParserPure)
import Options.Applicative.Builder(defaultPrefs)

import J.CLI

mainCLI :: IO ()
mainCLI = do
  join $ execParser opts
  putStrLn "Done."

mainManual :: [String] -> IO ()
mainManual args = case execParserPure defaultPrefs opts args of
  Success c -> c
  Failure f -> error ("error parsing arguments: \n" ++ show f)
  CompletionInvoked _ -> error "completion invoked?"

mainE :: Maybe [String] -> IO ()
mainE = maybe mainCLI mainManual

main :: IO ()
main = mainE Nothing
