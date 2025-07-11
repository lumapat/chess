module Main where

import CLI (CLIProcessor (..), nextTurn, runCLI)
import Chess.Board (chessBoard)
import Chess.Rules (Turn)

instance CLIProcessor () where
  showBoard _ = show chessBoard
  playMove _ (turn, s) = Right ((), nextTurn turn)

main :: IO ()
main = runCLI ()
