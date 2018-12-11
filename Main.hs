{-# language LambdaCase #-}

module Main where

import Prelude

import Control.Monad(join)
import Control.Monad.Reader
import Data.Functor(($>))
import Options.Applicative(ParserResult(..), execParser, execParserPure)
import Options.Applicative.Builder(defaultPrefs)
import System.Environment(getArgs)

import J.CLI

mainA :: [String] -> IO Bool
mainA args = case execParserPure defaultPrefs opts args of
        Success c -> putStrLn "args parse!" *> c
        Failure f -> error $
                "error parsing arguments: \n" ++ show f
        CompletionInvoked _ -> error
                "completion invoked?"

main :: IO ()
main = do
        args <- getArgs
        print args
        mainA args >>= putStrLn . \case
                True -> "Done."
                False -> "Errors occurred."
