module Types.Board
    ( Board (boardMaxCols, boardMaxRows)
    , BoardCoord (..)
    , (!?)
    , boardCol
    , boardRow
    , inBounds
    , newBoard
    ) where

import           Control.Applicative
import           Control.Monad       (mapM)
import           Data.Ix             (inRange)
import qualified Data.Vector         as DV

type BoardCol a = DV.Vector a
type BoardRow a = DV.Vector a

data BoardCoord = BoardCoord
    { bcRow :: Int
    , bcCol :: Int
    } deriving (Eq, Show)

data Board a = Board
    { boardMaxRows :: Int
    , boardMaxCols :: Int
    , boardMatrix  :: BoardRow (BoardCol a)
    } deriving (Eq, Show)

newBoard :: Int -> Int -> [[a]] -> a -> Board a
newBoard maxRows maxCols items defaultVal = Board maxRows maxCols matrix
    where matrix = DV.fromList $ take maxRows $ (f <$> items) ++ fillerRows
          f xs = DV.fromList $ take maxCols $ xs ++ fillerCols
          fillerCols = repeat defaultVal
          fillerRows = repeat $ DV.replicate maxCols defaultVal

(!?) :: Board a -> BoardCoord -> Maybe a
(!?) b (BoardCoord r c) = do
    row <- (boardMatrix b) DV.!? r
    row DV.!? c

boardRow :: Board a -> Int -> Maybe [a]
boardRow b r = do row <- (boardMatrix b) DV.!? r
                  return $ DV.toList row

boardCol :: Board a -> Int -> Maybe [a]
boardCol b c = do vals <- mapM (flip (DV.!?) c) (boardMatrix b)
                  return $ DV.toList vals

inBounds :: Board a -> BoardCoord -> Bool
inBounds b (BoardCoord r c) = validRow && validCol
    where validRow = inRange (0, boardMaxRows b) r
          validCol = inRange (0, boardMaxCols b) c
