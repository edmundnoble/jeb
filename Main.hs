{-# language LambdaCase #-}

module Main where

import Prelude

import Options.Applicative(ParserResult(..), execParserPure)
import Options.Applicative.Builder(defaultPrefs)
import System.Environment(getArgs)

import J.CLI
import J.ErrT

mainA :: [String] -> IO ()
mainA args = do
        (case execParserPure defaultPrefs opts args of
                Success act ->
                        runErrT act
                Failure f -> error $
                        "Error parsing arguments: \n" ++ show f
                CompletionInvoked _ -> error
                        "Completion invoked?") >>= \case
                Nothing -> putStrLn "Exited with errors."
                Just () -> putStrLn "Done."

main :: IO ()
main = getArgs >>= mainA
