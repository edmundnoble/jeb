{-# language LambdaCase #-}

module Main where

import Prelude

import Options.Applicative(ParserResult(..), execParserPure)
import Options.Applicative.Builder(defaultPrefs)
import System.Environment(getArgs)

import J.CLI

mainA :: [String] -> IO ()
mainA args = do
        case execParserPure defaultPrefs opts args of
                Success act -> act
                Failure f -> error $
                        "error parsing arguments: \n" ++ show f
                CompletionInvoked _ -> error
                        "completion invoked?"
        putStrLn "Done."

main :: IO ()
main = getArgs >>= mainA
