{-# LANGUAGE InstanceSigs #-}

module Chess.Rules
  ( Turn (..),
    Engine,
    newEngine,
    play,
  )
where

import Chess.Board (ChessBoard, chessBoard)
import Chess.Move (parseMove)
import Chess.Terminology (ChessColor (..), coloring)
import Data.Functor (($>))

data Turn = WhiteTurn | BlackTurn

newtype Engine = Engine ChessBoard

instance Show Engine where
  show :: Engine -> String
  show (Engine e) = show e

nextTurn WhiteTurn = BlackTurn
nextTurn BlackTurn = WhiteTurn

colorFromTurn BlackTurn = ChessBlack
colorFromTurn WhiteTurn = ChessWhite

play :: Engine -> (Turn, String) -> Either String (Engine, Turn)
play e (turn, s) =
  parseMove (coloring $ colorFromTurn turn) s $> (e, nextTurn turn)

newEngine :: Engine
newEngine = Engine chessBoard