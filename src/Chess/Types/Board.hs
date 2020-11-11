module Chess.Types.Board
    ( Board
    , (!?)
    , boardCol
    , boardRow
    , inBounds
    , maxCols
    , maxRows
    , newBoard
    ) where

import Chess.Types.Unit
import Control.Applicative
import Control.Monad (mapM)
import qualified Data.Vector as DV
import Data.Ix (inRange)

type BoardRow a = DV.Vector a
type BoardCol a = DV.Vector a

data Board a = Board
    { boardMaxRows :: Int
    , boardMaxCols :: Int
    , boardMatrix :: BoardRow (BoardCol a)
    } deriving (Eq, Show)

newBoard :: Int -> Int -> [[a]] -> a -> Board a
newBoard maxRows maxCols items defaultVal = Board maxRows maxCols matrix
    where matrix = DV.fromList $ take maxRows $ (f <$> items) ++ fillerRows
          f xs = DV.fromList $ take maxCols $ xs ++ fillerCols
          fillerCols = repeat defaultVal
          fillerRows = repeat $ DV.replicate maxCols defaultVal

maxRows = boardMaxRows
maxCols = boardMaxCols

(!?) :: Board a -> (Int, Int) -> Maybe a
(!?) b (r, c) = do
    row <- (boardMatrix b) DV.!? r
    row DV.!? c

boardRow :: Board a -> Int -> Maybe [a]
boardRow b r = do row <- (boardMatrix b) DV.!? r
                  return $ DV.toList row

boardCol :: Board a -> Int -> Maybe [a]
boardCol b c = do vals <- mapM (flip (DV.!?) c) (boardMatrix b)
                  return $ DV.toList vals

inBounds :: Board a -> (Int, Int) -> Bool
inBounds b (r,c) = validRow && validCol
    where validRow = inRange (0, boardMaxRows b) r
          validCol = inRange (0, boardMaxCols b) c