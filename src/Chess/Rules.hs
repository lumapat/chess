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
    squareAt,
    squaresFrom,
  )
import Chess.Move
  ( ChessMove (..),
    ChessPosition (..),
    DisambPosition,
    disambMatch,
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

    -- TODO: Use disamb in move?
    makeMove :: ChessMove -> Either String (Engine, Turn)
    makeMove (PieceMove piece pos _) = advance <$> movePiece e piece pos
    makeMove (PieceCapture piece disamb pos _) = advance <$> capturePiece e piece pos disamb
    makeMove _ = Right (e, nextTurn turn)

    advance :: Engine -> (Engine, Turn)
    advance e = (e, nextTurn turn)

capturePiece :: Engine -> ChessPiece -> ChessPosition -> DisambPosition -> Either String Engine
capturePiece (Engine board) piece dest disamb = validateCapture dest >>= flip capture attackers
  where
    validateCapture :: ChessPosition -> Either String ChessPosition
    validateCapture pos = checkPieceOn $ squareAt board pos
      where
        checkPieceOn (ChessBoardSquare Nothing _) = Left ("Nothing to capture on " ++ show pos)
        checkPieceOn (ChessBoardSquare (Just target) _)
          | pieceColor target == pieceColor piece = Left ("Cannot capture your own piece (" ++ show (pieceColor piece) ++ ")")
          | pieceType target == ChessKing = Left "Cannot capture an opponent king"
          | otherwise = Right pos

    attackers :: [ChessBoardSquare]
    attackers = findAttackers board piece dest

    capture :: ChessPosition -> [ChessBoardSquare] -> Either String Engine
    capture pos [sq] = Right $ Engine $ fst (moveTo board (squarePos sq) pos) -- TODO: Record capture or something
    capture pos [] = Left $ "No captures possible for " ++ show piece ++ " on " ++ show pos
    capture pos sqs = capture' $ filter (disambMatch disamb . squarePos) sqs
      where
        capture' (s1 : s2 : sqs) = Left $ "Multiple " ++ show piece ++ " can capture " ++ show pos
        capture' sqs' = capture pos sqs'

-- TODO: Need to check that the move is legal
--       [x] Square isn't filled
--       [ ] The next move won't put you in check
movePiece :: Engine -> ChessPiece -> ChessPosition -> Either String Engine
movePiece (Engine board) piece dest = validateMove dest >>= flip makeMove movers
  where
    validateMove :: ChessPosition -> Either String ChessPosition
    validateMove pos
      | isNothing (squarePiece (squareAt board pos)) = Right pos
      | otherwise = Left ("Cannot move to a filled square at " ++ show pos)
    movers :: [ChessBoardSquare]
    movers = findMovers board piece dest
    makeMove :: ChessPosition -> [ChessBoardSquare] -> Either String Engine
    makeMove pos [sq] = Right $ Engine $ fst (moveTo board (squarePos sq) pos)
    makeMove pos [] = Left $ "No moves possible for " ++ show piece ++ " " ++ show pos
    makeMove pos _ = Left $ "Multiple " ++ show piece ++ " can move to " ++ show pos

-- Most pieces will move the same as capture, for the very one exception of pawns
findAttackers :: ChessBoard -> ChessPiece -> ChessPosition -> [ChessBoardSquare]
findAttackers board piece@(ChessPiece color ChessPawn) fromPos = mconcat $ takeFirstNonEmpty piece <$> pawnSquares
  where
    -- Pawns can only move forward, so we can check if they're in their
    -- starting rank to check for 2 places (or 1 place for any other time)
    pawnSquares = take 1 . squaresFrom board fromPos <$> dir color
    dir ChessBlack = [BoardNE, BoardNW]
    dir ChessWhite = [BoardSE, BoardSW]
findAttackers board piece fromPos = findMovers board piece fromPos

-- Finds pieces that can move to the destination by applying
-- the opposite movement pattern to them. Usually, using the same
-- movement pattern as the piece would yield the squares that the
-- piece can use too
findMovers :: ChessBoard -> ChessPiece -> ChessPosition -> [ChessBoardSquare]
findMovers board piece@(ChessPiece color ChessPawn) fromPos = takeFirstNonEmpty piece pawnSquares
  where
    -- Pawns can only move forward, so we can check if they're in their
    -- starting rank to check for 2 places (or 1 place for any other time)
    pawnSquares = take (numPawnSquares fromPos) $ squaresFrom board fromPos (dir color)
    dir ChessBlack = BoardNorth
    dir ChessWhite = BoardSouth

    -- Only allow 2 square move if the pawn is on a starting rank
    numPawnSquares (ChessPosition _ rank)
      | startingRank color == rank = 2
      | otherwise = 1

    -- This is a bit confusing, but this isn't the actual starting rank of the pawn square
    -- Since findMovers starts from the DESTINATION square, we can safely assume that the only
    -- ranks that need to check for 2-square pawn moves are the middle ranks
    startingRank ChessBlack = R5
    startingRank ChessWhite = R4
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
    validate = mconcat . fmap (takeFirstNonEmpty piece)
findMovers board piece@(ChessPiece color ChessRook) fromPos = validate rookSquares
  where
    rookSquares = squaresFrom board fromPos <$> [BoardNorth, BoardSouth, BoardEast, BoardWest]

    validate :: [[ChessBoardSquare]] -> [ChessBoardSquare]
    validate = mconcat . fmap (takeFirstNonEmpty piece)
findMovers board piece@(ChessPiece color ChessQueen) fromPos = validate queenSquares
  where
    -- Combination of rook and bishop squares
    queenSquares =
      squaresFrom board fromPos
        <$> [ BoardNorth,
              BoardSouth,
              BoardEast,
              BoardWest,
              BoardNE,
              BoardNW,
              BoardSE,
              BoardSW
            ]

    validate :: [[ChessBoardSquare]] -> [ChessBoardSquare]
    validate = mconcat . fmap (takeFirstNonEmpty piece)
findMovers board piece@(ChessPiece color ChessKing) fromPos = mconcat $ takeFirstNonEmpty piece <$> kingSquares
  where
    -- All directions but only the first of each
    kingSquares =
      take 1 . squaresFrom board fromPos
        <$> [ BoardNorth,
              BoardSouth,
              BoardEast,
              BoardWest,
              BoardNE,
              BoardNW,
              BoardSE,
              BoardSW
            ]

-- This takes the first non-empty square since most pieces
-- collide with other pieces.
takeFirstNonEmpty :: ChessPiece -> [ChessBoardSquare] -> [ChessBoardSquare]
takeFirstNonEmpty piece (sq : sqs)
  | isNothing (squarePiece sq) = takeFirstNonEmpty piece sqs
  | squarePiece sq == Just piece = [sq]
  | otherwise = []
takeFirstNonEmpty _ [] = []

-- Debugging hook for the engine
-- Override this with any kind of debug you want
-- TODO: Create a debug stack tracer so we can print a breakdown
-- of how a move was calculated
debug :: Engine -> [String] -> String
debug (Engine board) _ = ""

newEngine :: Engine
newEngine = Engine chessBoard