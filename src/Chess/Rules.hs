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
    { chessBoard :: Board Piece
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
    , squarePiece :: Piece
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

validChessCoord :: ChessBoard -> ChessCoord -> Bool
validChessCoord b c = inBounds (chessBoard b) (toIntCoords c)

alternateColoredRow :: ChessColor -> [PieceClass] -> [Piece]
alternateColoredRow color pieces = Piece <$> colors <*> pieces
    where colors = alternatingColors <$> [1..]
          alternatingColors ix | even ix   = color
                               | otherwise = opposingChessColor color

blankRow :: [Piece]
blankRow = replicate maxFiles Open

peasantRow :: ChessColor -> [Piece]
peasantRow color = replicate maxFiles (Piece color Pawn)

royalRow :: ChessColor -> [Piece]
royalRow color = (Piece color) <$> [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- TODO: Use lens or another means to update??
-- Switch a square's file and rank
squareUp :: ChessCoord -> (Rank -> Rank) -> (File -> File) -> ChessCoord
squareUp c rankT fileT = ChessCoord (rankT $ boardRank c) (fileT $ boardFile c)

rankUp :: ChessCoord -> (Rank -> Rank) -> ChessCoord
rankUp c rankT = squareUp c rankT id

fileUp :: ChessCoord -> (File -> File) -> ChessCoord
fileUp c fileT = squareUp c id fileT

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
moves b c = do (Piece _ pieceClass) <- pieceAt b c
               return $ pieceMove pieceClass b c

pieceMove :: PieceClass -> ChessBoard -> ChessCoord -> [ChessCoord]
pieceMove King b s = filter (validChessCoord b) $ (toChessCoords . translate) <$> mvmts
    where mvmts = filter (/= (0,0)) $ range ((-1, -1), (1,1))
          translate (a,b) = (startR + a, startF + b)
          (startR, startF) = toIntCoords s

pieceMove Rook b s = up ++ down ++ left ++ right
    where takeValidRiderSquares ri fi sq = takeWhile (validChessCoord b) $ riderSquares ri fi sq
          up    = takeValidRiderSquares 1 0 s
          down  = takeValidRiderSquares (-1) 0 s
          right = takeValidRiderSquares 0 1 s
          left  = takeValidRiderSquares 0 (-1) s

pieceMove Bishop b s = ne ++ nw ++ se ++ sw
    where takeValidRiderSquares ri fi sq = takeWhile (validChessCoord b) $ riderSquares ri fi sq
          ne = takeValidRiderSquares 1 1 s
          nw = takeValidRiderSquares 1 (-1) s
          se = takeValidRiderSquares (-1) 1 s
          sw = takeValidRiderSquares (-1) (-1) s

pieceMove Queen b s = (pieceMove Rook b s) ++ (pieceMove Bishop b s)

pieceMove Knight b s = toChessCoords <$> knightSquares
    where f op (a,b) (c,d) = (op a c, op b d)
          mvmts = (f (*)) <$> [(-1,-1), (-1,1), (1,-1), (1,1)] <*> [(1,2), (2,1)]
          fromStart (ChessCoord (Rank startR) (File startF)) (a,b) = (startR + a, startF + b)
          knightSquares = filter (inBounds $ chessBoard b) $ (fromStart s) <$> mvmts

pieceMove Pawn b s = [rankUp s ((1 :: Rank) +)]

pieceAt :: ChessBoard -> ChessCoord -> Maybe Piece
pieceAt b (ChessCoord r f) = (chessBoard b) !? (fromEnum r, fromEnum f)

open :: ChessBoard -> ChessCoord -> Bool
open b c = pieceAt b c == Just Open
