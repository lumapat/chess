{-# LANGUAGE InstanceSigs #-}

module Chess.Rules
  ( Turn (..),
    Engine,
    newEngine,
    play,
  )
where

import Chess.Board (BoardDirection (..), ChessBoard, chessBoard, squaresFrom)
import Chess.Move (ChessMove (..), parseMove)
import Chess.Terminology (ChessColor (..), coloring)
import Data.Functor (($>))

data Turn = WhiteTurn | BlackTurn

newtype Engine = Engine ChessBoard

instance Show Engine where
  show :: Engine -> String
  show (Engine e) = show e

nextTurn :: Turn -> Turn
nextTurn WhiteTurn = BlackTurn
nextTurn BlackTurn = WhiteTurn

colorFromTurn :: Turn -> ChessColor
colorFromTurn BlackTurn = ChessBlack
colorFromTurn WhiteTurn = ChessWhite

play :: Engine -> (Turn, String) -> Either String (Engine, Turn)
play e@(Engine board) (turn, s) = parseMove color s >>= makeMove
  where
    -- Generate color
    color = coloring $ colorFromTurn turn

    makeMove :: ChessMove -> Either String (Engine, Turn)
    makeMove (PieceMove piece pos _) = Left $ show (squaresFrom board pos BoardNorth)
    makeMove _ = Right (e, nextTurn turn)

newEngine :: Engine
newEngine = Engine chessBoard