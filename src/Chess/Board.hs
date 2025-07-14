module Chess.Board
  ( BoardDirection (..),
    ChessBoard,
    ChessBoardSquare (..),
    chessBoard,
    squareAt,
    squaresFrom,
  )
where

import Chess.Move (ChessPosition (..))
import Chess.Terminology
  ( ChessColor (..),
    ChessFile (..),
    ChessPiece (..),
    ChessPieceType (..),
    ChessRank (..),
  )
import Data.List (intercalate)
import qualified Data.Vector as V

-- DO NOT EXPORT
newtype ChessBoardRawSquare = ChessBoardRawSquare (Maybe ChessPiece)

data ChessBoardSquare = ChessBoardSquare
  { squarePiece :: Maybe ChessPiece,
    squarePos :: ChessPosition
  }

instance Show ChessBoardRawSquare where
  show (ChessBoardRawSquare (Just piece)) = show piece
  show (ChessBoardRawSquare Nothing) = "_"

instance Show ChessBoardSquare where
  show (ChessBoardSquare (Just piece) _) = show piece
  show (ChessBoardSquare Nothing _) = "_"

newtype ChessBoard = ChessBoard (V.Vector (V.Vector ChessBoardRawSquare))

startingPromotedRow :: [ChessPieceType]
startingPromotedRow =
  [ ChessRook,
    ChessKnight,
    ChessBishop,
    ChessQueen,
    ChessKing,
    ChessBishop,
    ChessKnight,
    ChessRook
  ]

startingPawnRow :: [ChessPieceType]
startingPawnRow = replicate 8 ChessPawn

chessBoard :: ChessBoard
chessBoard = ChessBoard (V.fromList $ V.fromList <$> rawBoard)
  where
    rawBoard =
      [ ChessBoardRawSquare . Just . ChessPiece ChessBlack <$> startingPromotedRow,
        ChessBoardRawSquare . Just . ChessPiece ChessBlack <$> startingPawnRow,
        blankRow,
        blankRow,
        blankRow,
        blankRow,
        ChessBoardRawSquare . Just . ChessPiece ChessWhite <$> startingPawnRow,
        ChessBoardRawSquare . Just . ChessPiece ChessWhite <$> startingPromotedRow
      ]

    blankRow = replicate 8 (ChessBoardRawSquare Nothing)

-- TODO Test this
squareAt :: ChessBoard -> ChessPosition -> ChessBoardSquare
squareAt (ChessBoard v) pos@(ChessPosition file rank) = ChessBoardSquare sq pos
  where
    file' = fromEnum file - 1 -- Offset by 1 since enums are 1-indexed
    rank' = 8 - fromEnum rank -- Subtract from 8 since ranks are stored in reverse
    ChessBoardRawSquare sq = (V.!) ((V.!) v rank') file'

-- Directions of a square relative to a ChessPosition
-- North is towards black's side, and south is towards white's side
data BoardDirection
  = BoardNorth
  | BoardSouth
  | BoardEast
  | BoardWest
  | BoardNE
  | BoardNW
  | BoardSE
  | BoardSW
  | BoardL

-- TODO: Docs
squaresFrom :: ChessBoard -> ChessPosition -> BoardDirection -> [ChessBoardSquare]
squaresFrom board (ChessPosition file rank) = fmap (squareAt board) . squaresFrom'
  where
    squaresFrom' :: BoardDirection -> [ChessPosition]
    squaresFrom' BoardNorth = ChessPosition file <$> untilMax rank
    squaresFrom' BoardSouth = ChessPosition file <$> untilMin rank
    squaresFrom' BoardEast = flip ChessPosition rank <$> untilMax file
    squaresFrom' BoardWest = flip ChessPosition rank <$> untilMin file
    squaresFrom' BoardNE = zipWith ChessPosition (untilMax file) (untilMax rank)
    squaresFrom' BoardNW = zipWith ChessPosition (untilMin file) (untilMax rank)
    squaresFrom' BoardSE = zipWith ChessPosition (untilMax file) (untilMin rank)
    squaresFrom' BoardSW = zipWith ChessPosition (untilMin file) (untilMin rank)
    squaresFrom' _ = []

    -- Helpers
    -- Each helper auto excludes the inputted square
    untilMax :: (Bounded a, Enum a) => a -> [a]
    untilMax e = tail (enumFromTo e maxBound)
    untilMin :: (Bounded a, Enum a) => a -> [a]
    untilMin e = tail (reverse (enumFromTo minBound e))

instance Show ChessBoard where
  show (ChessBoard v) =
    intercalate
      "\n"
      ( unwords . fmap show . V.toList
          <$> V.toList v
      )
