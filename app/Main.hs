module Main where

import           CLI (CLIProcessor (..), nextTurn, runCLI)

instance CLIProcessor () where
    showBoard _ = "TODO"
    playMove _ (turn, s) = Right ((), nextTurn turn)

main :: IO ()
main = runCLI ()
