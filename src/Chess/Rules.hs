{-# LANGUAGE InstanceSigs #-}

module Chess.Rules
  ( Turn (..),
    Engine,
    debug,
    newEngine,
    play,
  )
where

import Chess.Board
  ( BoardDirection (..),
    ChessBoard,
    ChessBoardSquare (..),
    chessBoard,
    moveTo,
    squaresFrom,
  )
import Chess.Move
  ( ChessMove (..),
    ChessPosition (..),
    parseMove,
  )
import Chess.Terminology
  ( ChessColor (..),
    ChessFile (..),
    ChessPiece (..),
    ChessPieceType (..),
    ChessRank (..),
    coloring,
  )
import Data.Functor (($>))
import Data.Maybe (isJust, isNothing)

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
    movers :: [ChessBoardSquare]
    movers = findMovers board piece dest
    -- TODO: Detect if capture (error out) or not
    makeMove :: [ChessBoardSquare] -> Either String Engine
    makeMove [sq] = Right $ Engine $ fst (moveTo board (squarePos sq) dest)
    makeMove [] = Left $ "No moves possible for " ++ show piece ++ " " ++ show dest
    makeMove _ = Left $ "Multiple " ++ show piece ++ " can move to " ++ show dest

-- Finds pieces that can move to the destination by applying
-- the opposite movement pattern to them. Usually, using the same
-- movement pattern as the piece would yield the squares that the
-- piece can use too
findMovers :: ChessBoard -> ChessPiece -> ChessPosition -> [ChessBoardSquare]
findMovers board piece@(ChessPiece color ChessPawn) fromPos = validate pawnSquares
  where
    -- Pawns can only move forward, so we can check if they're in their
    -- starting rank to check for 2 places (or 1 place for any other time)
    pawnSquares = take 2 $ squaresFrom board fromPos (dir color)
    dir ChessBlack = BoardNorth
    dir ChessWhite = BoardSouth

    validate :: [ChessBoardSquare] -> [ChessBoardSquare]
    validate (sq : sqs)
      | isNothing (squarePiece sq) = validate sqs
      | squarePiece sq == Just piece = [sq]
      | otherwise = []
    validate [] = []

    startingRank ChessBlack = R7
    startingRank ChessWhite = R2
findMovers board piece@(ChessPiece color ChessKnight) fromPos = validate knightSquares
  where
    knightSquares = squaresFrom board fromPos BoardL

    -- Knights can jump, so no need for any collision detection
    validate :: [ChessBoardSquare] -> [ChessBoardSquare]
    validate = filter ((== Just piece) . squarePiece)
findMovers board piece@(ChessPiece color ChessBishop) fromPos = validate bishopSquares
  where
    bishopSquares = squaresFrom board fromPos <$> [BoardNE, BoardNW, BoardSE, BoardSW]

    validate :: [[ChessBoardSquare]] -> [ChessBoardSquare]
    validate = mconcat . fmap validate'

    -- TODO: Should consolidate some collision detection and empty square detection
    validate' :: [ChessBoardSquare] -> [ChessBoardSquare]
    validate'
      (sq : sqs)
        | isNothing (squarePiece sq) = validate' sqs
        | squarePiece sq == Just piece = [sq]
        | otherwise = []
    validate' [] = []
findMovers _ _ _ = []

-- Debugging hook for the engine
-- Override this with any kind of debug you want
-- TODO: Create a debug stack tracer so we can print a breakdown
-- of how a move was calculated
debug :: Engine -> [String] -> String
debug (Engine board) _ = ""

newEngine :: Engine
newEngine = Engine chessBoard