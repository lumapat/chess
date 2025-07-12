module Main where

import CLI (CLIProcessor (..), runCLI)
import qualified Chess.Rules as Rules

instance CLIProcessor Rules.Engine where
  showBoard e = show e
  playMove e (turn, s) = Rules.play e (turn, s)

main :: IO ()
main = runCLI Rules.newEngine
