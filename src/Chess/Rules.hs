{-# LANGUAGE InstanceSigs #-}

module Chess.Rules
  ( Turn (..),
    Engine,
    newEngine,
    play,
  )
where

import Chess.Board (BoardDirection (..), ChessBoard, ChessBoardSquare (..), chessBoard, moveTo, squaresFrom)
import Chess.Move (ChessMove (..), ChessPosition, parseMove)
import Chess.Terminology (ChessColor (..), ChessPiece (..), ChessPieceType (..), coloring)
import Data.Functor (($>))
import Data.Maybe (isJust)

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
play e (turn, s) = parseMove color s >>= makeMove
  where
    color = colorFromTurn turn

    makeMove :: ChessMove -> Either String (Engine, Turn)
    makeMove (PieceMove piece pos _) = advance <$> movePiece e piece pos
    makeMove _ = Right (e, nextTurn turn)

    advance :: Engine -> (Engine, Turn)
    advance e = (e, nextTurn turn)

-- TODO: Need to check that the move is legal
movePiece :: Engine -> ChessPiece -> ChessPosition -> Either String Engine
movePiece (Engine board) piece dest = makeMove movers
  where
    movers =
      take 1 $
        filter (isJust . squarePiece) $
          findMovers board piece dest
    -- TODO: Detect if capture (error out) or not
    makeMove :: [ChessBoardSquare] -> Either String Engine
    makeMove [sq] = Right $ Engine $ fst (moveTo board (squarePos sq) dest)
    makeMove _ = Left $ "Move: " ++ show piece ++ " " ++ show dest ++ " NYI"

findMovers :: ChessBoard -> ChessPiece -> (ChessPosition -> [ChessBoardSquare])
findMovers board (ChessPiece color ChessPawn) = flip (squaresFrom board) (dir color)
  where
    dir ChessBlack = BoardNorth
    dir ChessWhite = BoardSouth
findMovers _ _ = const []

newEngine :: Engine
newEngine = Engine chessBoard