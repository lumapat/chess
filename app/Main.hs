module Main where

import CLI (CLIDebug (..), CLIProcessor (..), runCLI)
import qualified Chess.Rules as Rules

instance CLIProcessor Rules.Engine where
  showBoard e = show e
  playMove e (turn, s) = Rules.play e (turn, s)

instance CLIDebug Rules.Engine where
  debug1 = Rules.debug

main :: IO ()
main = runCLI Rules.newEngine
