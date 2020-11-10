{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Board
    ( Board (..)
    , ChessCoord (..)
    , File (..)
    , Piece (..)
    , Rank (..)
    , Square (..)
    , cleanBoard
    , moves
    , pieceMove
    ) where

import Control.Applicative
import qualified Data.Vector as DV
import Data.Ix (range)

data ChessColor = White
                | Black
                deriving (Enum, Eq, Ord, Show)

opposingChessColor :: ChessColor -> ChessColor
opposingChessColor Black = White
opposingChessColor White = Black

-- TODO: Rename to ChessPiece
data Piece = Piece ChessColor PieceClass
           | Open
           deriving (Eq)

-- TODO: Rename to ChessPieceClass
data PieceClass = King
                | Queen
                | Rook
                | Bishop
                | Knight
                | Pawn
                deriving (Eq, Show)

instance Show Piece where
    show Open = " "

    show (Piece Black King) = "♚"
    show (Piece Black Queen) = "♛"
    show (Piece Black Rook) = "♜"
    show (Piece Black Bishop) = "♝"
    show (Piece Black Knight) = "♞"
    show (Piece Black Pawn) = "♟"

    show (Piece White King) = "♔"
    show (Piece White Queen) = "♕"
    show (Piece White Rook) = "♖"
    show (Piece White Bishop) = "♗"
    show (Piece White Knight) = "♘"
    show (Piece White Pawn) = "♙"

instance Enum PieceClass where
    fromEnum King = 1
    fromEnum Queen = 2
    fromEnum Rook = 3
    fromEnum Bishop = 4
    fromEnum Knight = 5
    fromEnum Pawn = 6

    toEnum 1 = King
    toEnum 2 = Queen
    toEnum 3 = Rook
    toEnum 4 = Bishop
    toEnum 5 = Knight
    toEnum 6 = Pawn

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

cleanBoard :: Board
cleanBoard = Board maxRanks maxFiles $ DV.fromList $ alternatingColorsOf
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


moves :: Board -> ChessCoord -> Maybe [ChessCoord]
moves b c = do (Piece _ pieceClass) <- pieceAt b c
               return $ pieceMove pieceClass b c

pieceMove :: PieceClass -> Board -> ChessCoord -> [ChessCoord]
pieceMove King b s = filter (inBounds b) $ ((uncurry ChessCoord) . ff) <$> (filter (/= (0,0)) $ range ((-1, -1), (1,1)))
    where f = (+ (boardRank s)) . Rank
          f' = (+ (boardFile s)) . File
          ff (rank, file) = (f rank, f' file)

pieceMove Rook b s = up ++ down ++ left ++ right
    where takeValidRiderSquares ri fi sq = (takeWhile (inBounds b)) $ riderSquares ri fi sq
          up    = takeValidRiderSquares 1 0 s
          down  = takeValidRiderSquares (-1) 0 s
          right = takeValidRiderSquares 0 1 s
          left  = takeValidRiderSquares 0 (-1) s

pieceMove Bishop b s = ne ++ nw ++ se++ sw
    where takeValidRiderSquares ri fi sq = (takeWhile (inBounds b)) $ riderSquares ri fi sq
          ne = takeValidRiderSquares 1 1 s
          nw = takeValidRiderSquares 1 (-1) s
          se = takeValidRiderSquares (-1) 1 s
          sw = takeValidRiderSquares (-1) (-1) s

pieceMove Queen b s = (pieceMove Rook b s) ++ (pieceMove Bishop b s)

pieceMove Knight b s = filter (inBounds b) $ newSq <$> mvmts
    where f op (a,b) (c,d) = (op a c, op b d)
          mvmts = (f (*)) <$> [(-1,-1), (-1,1), (1,-1), (1,1)] <*> [(1,2), (2,1)]
          newSq (a, b) = squareUp s (+ Rank b) (+ File a)

pieceMove Pawn b s = [rankUp s ((1 :: Rank) +)]
