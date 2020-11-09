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

data Piece = Open
           | King
           | Queen
           | Rook
           | Bishop
           | Knight
           | Pawn
           deriving (Eq, Show)

instance Enum Piece where
    fromEnum Open = 0
    fromEnum King = 1
    fromEnum Queen = 2
    fromEnum Rook = 3
    fromEnum Bishop = 4
    fromEnum Knight = 5
    fromEnum Pawn = 6

    toEnum 0 = Open
    toEnum 1 = King
    toEnum 2 = Queen
    toEnum 3 = Rook
    toEnum 4 = Bishop
    toEnum 5 = Knight
    toEnum 6 = Pawn

data Board = Board
    { boardSquares :: DV.Vector (DV.Vector Piece)
    , boardMaxFile :: File
    , boardMinFile :: File
    , boardMaxRank :: Rank
    , boardMinRank :: Rank
    } deriving (Eq, Show)

cleanBoard :: Board
cleanBoard = Board
    { boardSquares = DV.replicate 8 (DV.replicate 8 Open)
    , boardMaxFile = File 7
    , boardMinFile = File 0
    , boardMaxRank = Rank 7
    , boardMinRank = Rank 0
    }

newtype File = File Int deriving (Enum, Eq, Num, Ord, Show)
newtype Rank = Rank Int deriving (Enum, Eq, Num, Ord, Show)

data Square = Square
    { boardFile :: File
    , boardRank :: Rank
    } deriving (Eq, Ord, Show)

square :: Int -> Int -> Square
square file rank = Square (File file) (Rank rank)

squareIndices :: Square -> (Int, Int)
squareIndices (Square (File col) (Rank row)) = (col, row)

-- TODO: Use lens or another means to update??
-- Switch a square's file and rank
squareUp :: Square -> (File -> File) -> (Rank -> Rank) -> Square
squareUp s fileT rankT = Square (fileT $ boardFile s) (rankT $ boardRank s)

rankUp :: Square -> (Rank -> Rank) -> Square
rankUp sq rankT = squareUp sq id rankT

fileUp :: Square -> (File -> File) -> Square
fileUp sq fileT = squareUp sq fileT id

-- TODO: Docs
-- Rider squares are squares going in multiple directions
riderSquares :: (File, Rank) -> Square -> [Square]
riderSquares (fileInc, rankInc) s = (uncurry Square) <$> (drop 1 $ zip files ranks)
    where startingFile = boardFile s
          startingRank = boardRank s
          files = [startingFile, startingFile + fileInc..]
          ranks = [startingRank, startingRank + rankInc..]

inBounds :: Board -> Square -> Bool
inBounds b s | (boardFile s) < (boardMinFile b) = False
             | (boardFile s) > (boardMaxFile b) = False
             | (boardRank s) < (boardMinRank b) = False
             | (boardRank s) > (boardMaxRank b) = False
             | otherwise                        = True

open :: Board -> Square -> Bool
open b s = ((boardSquares b) DV.! col DV.! row) == Open
    where (col, row) = squareIndices s

pieceAt :: Square -> Board -> Maybe Piece
pieceAt s b | not $ inBounds b s = Nothing
            | otherwise          = Just $ (boardSquares b) DV.! (fromEnum $ boardFile s) DV.! (fromEnum $ boardRank s)


moves :: Board -> Square -> Maybe [Square]
moves b s = do piece <- pieceAt s b
               return $ pieceMove piece b s

pieceMove :: Piece -> Board -> Square -> [Square]
pieceMove Open _ _ = []
pieceMove King b s = ((uncurry Square) . ff) <$> (filter (/= (0,0)) $ range ((-1, -1), (1,1)))
    where f = (+ (boardRank s)) . Rank
          f' = (+ (boardFile s)) . File
          ff (file, rank) = (f' file, f rank)

pieceMove Rook b s = up ++ down ++ left ++ right
    where takeValidBy inc = takeWhile (inBounds b) $ riderSquares inc s
          up = takeValidBy (File 0, Rank 1)
          down = takeValidBy (File 0, Rank (-1))
          right = takeValidBy (File 1, Rank 0)
          left = takeValidBy (File (-1), Rank 0)

pieceMove Bishop b s = ne ++ nw ++ se++ sw
    where takeValidBy inc = takeWhile (inBounds b) $ riderSquares inc s
          ne = takeValidBy (File 1, Rank 1)
          nw = takeValidBy (File (-1), Rank 1)
          se = takeValidBy (File 1, Rank (-1))
          sw = takeValidBy (File (-1), Rank (-1))

pieceMove Queen b s = (pieceMove Rook b s) ++ (pieceMove Bishop b s)

pieceMove Knight b s = newSq <$> mvmts
    where f op (a,b) (c,d) = (op a c, op b d)
          mvmts = (f (*)) <$> [(-1,-1), (-1,1), (1,-1), (1,1)] <*> [(1,2), (2,1)]
          newSq (a, b) = squareUp s (+ File a) (+ Rank b)

pieceMove Pawn b s = [rankUp s ((1 :: Rank) +)]
