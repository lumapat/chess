{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Board
    ( Board (..)
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

data ChessColor = Black
                | White
                deriving (Eq, Show)

data Piece = Piece ChessColor PieceClass
           | Open
           deriving (Eq)

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
    , boardSquares :: DV.Vector (DV.Vector Piece)
    } deriving (Eq, Show)

maxRanks = 8
maxFiles = 8

cleanBoard :: Board
cleanBoard = Board maxRanks maxFiles $ DV.fromList
    [ royalRow Black
    , peasantRow Black
    , blankRow
    , blankRow
    , blankRow
    , blankRow
    , peasantRow White
    , royalRow White
    ]

blankRow :: DV.Vector Piece
blankRow = DV.replicate maxFiles Open

peasantRow :: ChessColor -> DV.Vector Piece
peasantRow color = DV.replicate maxFiles (Piece color Pawn)

royalRow :: ChessColor -> DV.Vector Piece
royalRow color = DV.fromList $ (Piece color) <$> [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

newtype File = File Int deriving (Enum, Eq, Num, Ord, Show)
newtype Rank = Rank Int deriving (Enum, Eq, Num, Ord, Show)

data Square = Square
    { boardRank :: Rank
    , boardFile :: File
    } deriving (Eq, Ord, Show)

square :: Int -> Int -> Square
square rank file = Square (Rank rank) (File file)

squareIndices :: Square -> (Int, Int)
squareIndices (Square (Rank row) (File col)) = (row, col)

-- TODO: Use lens or another means to update??
-- Switch a square's file and rank
squareUp :: Square -> (Rank -> Rank) -> (File -> File) -> Square
squareUp s rankT fileT = Square (rankT $ boardRank s) (fileT $ boardFile s)

rankUp :: Square -> (Rank -> Rank) -> Square
rankUp sq rankT = squareUp sq rankT id

fileUp :: Square -> (File -> File) -> Square
fileUp sq fileT = squareUp sq id fileT

-- TODO: Docs
-- Rider squares are squares going in multiple directions
riderSquares :: Int -> Int -> Square -> [Square]
riderSquares r f s = (uncurry Square) <$> (drop 1 $ zip ranks files)
    where rankInc = Rank r
          fileInc = File f
          startingRank = boardRank s
          startingFile = boardFile s
          ranks = [startingRank, startingRank + rankInc..]
          files = [startingFile, startingFile + fileInc..]

inBounds :: Board -> Square -> Bool
inBounds b s | (fromEnum $ boardFile s) < 0                 = False
             | (fromEnum $ boardFile s) > (boardMaxFiles b) = False
             | (fromEnum $ boardRank s) < 0                 = False
             | (fromEnum $ boardRank s) > (boardMaxRanks b) = False
             | otherwise                                    = True

open :: Board -> Square -> Bool
open b s = ((boardSquares b) DV.! col DV.! row) == Open
    where (col, row) = squareIndices s

pieceAt :: Board -> Square -> Maybe Piece
pieceAt b s | not $ inBounds b s = Nothing
            | otherwise          = Just $ (boardSquares b) DV.! (fromEnum $ boardRank s) DV.! (fromEnum $ boardFile s)


moves :: Board -> Square -> Maybe [Square]
moves b s = do (Piece _ pieceClass) <- pieceAt b s
               return $ pieceMove pieceClass b s

pieceMove :: PieceClass -> Board -> Square -> [Square]
pieceMove King b s = ((uncurry Square) . ff) <$> (filter (/= (0,0)) $ range ((-1, -1), (1,1)))
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

pieceMove Knight b s = newSq <$> mvmts
    where f op (a,b) (c,d) = (op a c, op b d)
          mvmts = (f (*)) <$> [(-1,-1), (-1,1), (1,-1), (1,1)] <*> [(1,2), (2,1)]
          newSq (a, b) = squareUp s (+ Rank b) (+ File a)

pieceMove Pawn b s = [rankUp s ((1 :: Rank) +)]
