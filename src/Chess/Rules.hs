{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Rules
    ( ChessBoard (..)
    , ChessCoord (..)
    , moves
    , startingBoard
    ) where

import Chess.Types.Board
import Chess.Types.Unit
import Data.Ix (range)

data ChessBoard = ChessBoard
    { chessBoard :: Board ChessPiece
    } deriving (Eq, Show)

maxRanks = 8
maxFiles = 8

newtype File = File Int deriving (Enum, Eq, Num, Ord, Show)
newtype Rank = Rank Int deriving (Enum, Eq, Num, Ord, Show)

data ChessCoord = ChessCoord
    { boardRank :: Rank
    , boardFile :: File
    } deriving (Eq, Ord, Show)

toIntCoords :: ChessCoord -> (Int, Int)
toIntCoords c = ((fromEnum $ boardRank c), (fromEnum $ boardFile c))

toChessCoords :: (Int, Int) -> ChessCoord
toChessCoords (r, c) = ChessCoord
    { boardRank = toEnum r
    , boardFile = toEnum c
    }

data Square = Square
    { squareColor :: ChessColor
    , squareChessPiece :: ChessPiece
    } deriving (Eq, Show)

startingBoard :: ChessBoard
startingBoard = ChessBoard $ newBoard maxRanks maxFiles boardSetup Open
    where alternatingColorsOf = zipWith alternateColoredRow (cycle [White, Black])
          boardSetup = [ royalRow Black
                       , peasantRow Black
                       , blankRow
                       , blankRow
                       , blankRow
                       , blankRow
                       , peasantRow White
                       , royalRow White
                       ]

pawnStartRank :: ChessColor -> Rank
pawnStartRank White = 1
pawnStartRank Black = 6

validChessCoord :: ChessBoard -> ChessCoord -> Bool
validChessCoord b c = inBounds (chessBoard b) (toIntCoords c)

alternateColoredRow :: ChessColor -> [ChessPieceClass] -> [ChessPiece]
alternateColoredRow color pieces = ChessPiece <$> colors <*> pieces
    where colors = alternatingColors <$> [1..]
          alternatingColors ix | even ix   = color
                               | otherwise = opposingChessColor color

blankRow :: [ChessPiece]
blankRow = replicate maxFiles Open

peasantRow :: ChessColor -> [ChessPiece]
peasantRow color = replicate maxFiles (ChessPiece color Pawn)

royalRow :: ChessColor -> [ChessPiece]
royalRow color = (ChessPiece color) <$> [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- TODO: Docs
-- Rider squares are squares going in multiple directions
riderSquares :: Int -> Int -> ChessCoord -> [ChessCoord]
riderSquares r f s = drop 1 $ zipWith ChessCoord ranks files
    where rankInc = Rank r
          fileInc = File f
          startingRank = boardRank s
          startingFile = boardFile s
          ranks = [startingRank, startingRank + rankInc..]
          files = [startingFile, startingFile + fileInc..]

moves :: ChessBoard -> ChessCoord -> Maybe [ChessCoord]
moves b c = do p  <- pieceAt b c
               return $ pieceMove p b c

pieceMove :: ChessPiece -> ChessBoard -> ChessCoord -> [ChessCoord]
pieceMove (ChessPiece _ King) b s = filter (validChessCoord b) $ (toChessCoords . translate) <$> mvmts
    where mvmts = filter (/= (0,0)) $ range ((-1, -1), (1,1))
          translate (a,b) = (startR + a, startF + b)
          (startR, startF) = toIntCoords s

pieceMove (ChessPiece _ Rook) b s = up ++ down ++ left ++ right
    where takeValidRiderSquares ri fi sq = takeWhile (validChessCoord b) $ riderSquares ri fi sq
          up    = takeValidRiderSquares 1 0 s
          down  = takeValidRiderSquares (-1) 0 s
          right = takeValidRiderSquares 0 1 s
          left  = takeValidRiderSquares 0 (-1) s

pieceMove (ChessPiece _ Bishop) b s = ne ++ nw ++ se ++ sw
    where takeValidRiderSquares ri fi sq = takeWhile (validChessCoord b) $ riderSquares ri fi sq
          ne = takeValidRiderSquares 1 1 s
          nw = takeValidRiderSquares 1 (-1) s
          se = takeValidRiderSquares (-1) 1 s
          sw = takeValidRiderSquares (-1) (-1) s

pieceMove (ChessPiece color Queen) b c = rookMoves ++ bishopMoves
    where rookMoves = pieceMove (ChessPiece color Rook) b c
          bishopMoves = pieceMove (ChessPiece color Bishop) b c

pieceMove (ChessPiece _ Knight) b s = toChessCoords <$> knightSquares
    where f op (a,b) (c,d) = (op a c, op b d)
          mvmts = (f (*)) <$> [(-1,-1), (-1,1), (1,-1), (1,1)] <*> [(1,2), (2,1)]
          fromStart (ChessCoord (Rank startR) (File startF)) (a,b) = (startR + a, startF + b)
          knightSquares = filter (inBounds $ chessBoard b) $ (fromStart s) <$> mvmts

pieceMove (ChessPiece color Pawn) b c = [toChessCoords (rank+1, file)]
    where (rank, file) = toIntCoords c

pieceAt :: ChessBoard -> ChessCoord -> Maybe ChessPiece
pieceAt b (ChessCoord r f) = (chessBoard b) !? (fromEnum r, fromEnum f)

open :: ChessBoard -> ChessCoord -> Bool
open b c = pieceAt b c == Just Open
