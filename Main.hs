{-# language LambdaCase #-}

module Main where

import Prelude

import Control.Monad(join)
import Options.Applicative(ParserResult(..), execParser, execParserPure)
import Options.Applicative.Builder(defaultPrefs)

import J.CLI

mainCLI :: IO Bool
mainCLI = join $ execParser opts

mainManual :: [String] -> IO Bool
mainManual args = case execParserPure defaultPrefs opts args of
  Success c -> c
  Failure f -> error ("error parsing arguments: \n" ++ show f)
  CompletionInvoked _ -> error "completion invoked?"

mainE :: Maybe [String] -> IO Bool
mainE = maybe mainCLI mainManual

main :: IO ()
main = mainE Nothing >>= putStrLn . \case
  True -> "Done."
  False -> "Errors occurred."
