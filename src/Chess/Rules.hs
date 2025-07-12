{-# LANGUAGE InstanceSigs #-}

module Chess.Rules
  ( Turn (..),
    Engine,
    newEngine,
    play,
  )
where

import Chess.Board (ChessBoard, chessBoard)

data Turn = WhiteTurn | BlackTurn

newtype Engine = Engine ChessBoard

instance Show Engine where
  show :: Engine -> String
  show (Engine e) = show e

nextTurn WhiteTurn = BlackTurn
nextTurn BlackTurn = WhiteTurn

play :: Engine -> (Turn, String) -> Either String (Engine, Turn)
play e (turn, s) = Right (e, nextTurn turn)

newEngine :: Engine
newEngine = Engine chessBoard

showBoard :: Engine -> String
showBoard _ = "TODO"
