{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Types.Board
    ( Board (..)
    , ChessCoord (..)
    , File (..)
    , Rank (..)
    , Square (..)
    , inBounds
    , pieceAt
    , rankUp
    , riderSquares
    , squareUp
    , startingBoard
    ) where

import Chess.Types.Unit
import Control.Applicative
import qualified Data.Vector as DV
import Data.Ix (range)

data Board = Board
    { boardMaxRanks :: Int
    , boardMaxFiles :: Int
    , boardSquares :: DV.Vector (DV.Vector Square)
    } deriving (Eq, Show)

maxRanks = 8
maxFiles = 8

newtype File = File Int deriving (Enum, Eq, Num, Ord, Show)
newtype Rank = Rank Int deriving (Enum, Eq, Num, Ord, Show)

data ChessCoord = ChessCoord
    { boardRank :: Rank
    , boardFile :: File
    } deriving (Eq, Ord, Show)

data Square = Square
    { squareColor :: ChessColor
    , squarePiece :: Piece
    } deriving (Eq, Show)

startingBoard :: Board
startingBoard = Board maxRanks maxFiles $ DV.fromList $ alternatingColorsOf
    [ royalRow Black
    , peasantRow Black
    , blankRow
    , blankRow
    , blankRow
    , blankRow
    , peasantRow White
    , royalRow White
    ] where alternatingColorsOf = zipWith alternateColoredRow (cycle [White, Black])

alternateColoredRow :: ChessColor -> DV.Vector Piece -> DV.Vector Square
alternateColoredRow color pieces = DV.zipWith Square colors pieces where
    colors = DV.generate maxFiles alternatingColors
    alternatingColors ix | even ix   = color
                         | otherwise = opposingChessColor color

blankRow :: DV.Vector Piece
blankRow = DV.replicate maxFiles Open

peasantRow :: ChessColor -> DV.Vector Piece
peasantRow color = DV.replicate maxFiles (Piece color Pawn)

royalRow :: ChessColor -> DV.Vector Piece
royalRow color = DV.fromList $ (Piece color) <$> [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- square :: Int -> Int -> Square
-- square rank file = Square (Rank rank) (File file)

squareIndices :: ChessCoord -> (Int, Int)
squareIndices (ChessCoord (Rank row) (File col)) = (row, col)

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

inBounds :: Board -> ChessCoord -> Bool
inBounds b c | (fromEnum $ boardFile c) < 0                 = False
             | (fromEnum $ boardFile c) > (boardMaxFiles b) = False
             | (fromEnum $ boardRank c) < 0                 = False
             | (fromEnum $ boardRank c) > (boardMaxRanks b) = False
             | otherwise                                    = True

open :: Board -> ChessCoord -> Bool
open b c = squarePiece ((boardSquares b) DV.! row DV.! col) == Open
    where (row, col) = squareIndices c

pieceAt :: Board -> ChessCoord -> Maybe Piece
pieceAt b c | not $ inBounds b c = Nothing
            | otherwise          = Just $ squarePiece $ (boardSquares b) DV.! (fromEnum $ boardRank c) DV.! (fromEnum $ boardFile c)

