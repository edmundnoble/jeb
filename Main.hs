{-# language LambdaCase #-}
{-# language ViewPatterns #-}

module Main where

import Prelude

import Data.Functor(($>))
import Options.Applicative(ParserResult(..), execParserPure)
import Options.Applicative.Builder(defaultPrefs)
import System.Environment(getArgs)
import System.IO (stdout)
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Jeb.CLI
import Jeb.ErrT

mainA :: [String] -> IO ()
mainA args = do
        (case execParserPure defaultPrefs opts args of
                Success act ->
                        terminateErrT act
                Failure f -> error $
                        "Error parsing arguments: \n" ++ show f
                CompletionInvoked _ -> error
                        "Completion invoked?") >>= \case
                Nothing -> putStrLn "Exited with errors."
                Just () -> putStrLn "Done."

printErr :: PP.Doc -> IO ()
printErr ((<> PP.hardline) -> e) = do
        isTty <- queryTerminal stdOutput
        if isTty
        then PP.putDoc e
        else PP.displayIO stdout (PP.renderCompact e)

terminateErrT :: ErrT IO () -> IO (Maybe ())
terminateErrT (ErrT fea) = fea >>= \case
        Left e -> printErr e $> Nothing
        Right () -> pure $ Just ()

main :: IO ()
main = getArgs >>= mainA
