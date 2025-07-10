module Main where

import CLI (CLIProcessor (..), nextTurn, runCLI)
import Chess.Rules (Turn)

instance CLIProcessor () where
  showBoard _ = "TODO"
  playMove _ (turn, s) = Right ((), nextTurn turn)

main :: IO ()
main = runCLI ()
