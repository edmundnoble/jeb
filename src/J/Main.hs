module J.Main(main) where

import Prelude

import Control.Monad(join)
import Options.Applicative(execParser)

import J.CLI

main :: IO ()
main = do
  join $ execParser opts
  putStrLn "Done."
