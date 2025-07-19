module Chess.Board
  ( BoardDirection (..),
    ChessBoard,
    ChessBoardSquare (..),
    chessBoard,
    moveTo,
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
import Control.Monad (msum)
import Data.List (intercalate)
import Data.List.Extra (chunksOf)
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

newtype ChessBoard = ChessBoard (V.Vector ChessBoardRawSquare)

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

-- Maximum number of squares in a single row on a chessboard
maxSquares :: Int
maxSquares = 8

startingPawnRow :: [ChessPieceType]
startingPawnRow = replicate maxSquares ChessPawn

chessBoard :: ChessBoard
chessBoard = ChessBoard (V.fromList rawBoard)
  where
    rawBoard =
      msum
        [ ChessBoardRawSquare . Just . ChessPiece ChessBlack <$> startingPromotedRow,
          ChessBoardRawSquare . Just . ChessPiece ChessBlack <$> startingPawnRow,
          blankRow,
          blankRow,
          blankRow,
          blankRow,
          ChessBoardRawSquare . Just . ChessPiece ChessWhite <$> startingPawnRow,
          ChessBoardRawSquare . Just . ChessPiece ChessWhite <$> startingPromotedRow
        ]

    blankRow = replicate maxSquares (ChessBoardRawSquare Nothing)

-- Accessors to the raw board
-- DO NOT EXPORT
toFileIndex :: ChessFile -> Int
toFileIndex file = fromEnum file - 1 -- Offset by 1 since enums are 1-indexed

toRankIndex :: ChessRank -> Int
toRankIndex rank = 8 - fromEnum rank -- Subtract from 8 since ranks are stored in reverse

toRawIndex :: ChessPosition -> Int
toRawIndex (ChessPosition file rank) = toRankIndex rank * maxSquares + toFileIndex file

-- TODO Test this
squareAt :: ChessBoard -> ChessPosition -> ChessBoardSquare
squareAt (ChessBoard v) pos = ChessBoardSquare sq pos
  where
    ChessBoardRawSquare sq = (V.!) v (toRawIndex pos)

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
squaresFrom board pos@(ChessPosition file rank) = fmap (squareAt board) . squaresFrom'
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
    -- squaresFrom' BoardL = filter (isValidL pos) $ makeL pos <$> lOffsets
    squaresFrom' BoardL = makeL pos <$> lOffsets

    -- L offsets
    lOffsets :: [(Int, Int)]
    lOffsets =
      tupMult
        <$> [(2, 1), (1, 2)]
        <*> [(1, 1), (1, -1), (-1, 1), (-1, -1)]
      where
        tupMult (a, b) (c, d) = (a * c, b * d)

    makeL :: ChessPosition -> (Int, Int) -> ChessPosition
    makeL (ChessPosition file rank) (df, dr) = ChessPosition file' rank'
      where
        file' = toEnum (fromEnum file + df)
        rank' = toEnum (fromEnum rank + dr)

    isValidL :: ChessPosition -> ChessPosition -> Bool
    isValidL (ChessPosition f1 r1) (ChessPosition f2 r2) = d == (1, 2) || d == (2, 1)
      where
        d = (fDelta, rDelta)
        fDelta = abs $ fromEnum f1 - fromEnum f2
        rDelta = abs $ fromEnum r1 - fromEnum r2

    -- Helpers
    -- Each helper auto excludes the inputted square
    untilMax :: (Bounded a, Enum a) => a -> [a]
    untilMax e = tail (enumFromTo e maxBound)
    untilMin :: (Bounded a, Enum a) => a -> [a]
    untilMin e = tail (reverse (enumFromTo minBound e))

-- Private DO NOT EXPORT
writeSquares :: ChessBoard -> [ChessBoardSquare] -> ChessBoard
writeSquares (ChessBoard board) squares = ChessBoard $ (V.//) board (toRawSquare <$> squares)
  where
    toRawSquare :: ChessBoardSquare -> (Int, ChessBoardRawSquare)
    toRawSquare (ChessBoardSquare piece pos) = (toRawIndex pos, ChessBoardRawSquare piece)

-- Moves a piece that's on a source square to a destination square
-- If a piece exists on that destination square, then return a capture
-- TODO: Replace with State monad
moveTo :: ChessBoard -> ChessPosition -> ChessPosition -> (ChessBoard, Maybe ChessPiece)
moveTo b srcPos = overwriteMaybe (squareAt b srcPos)
  where
    piece = squareAt b srcPos

    overwriteMaybe :: ChessBoardSquare -> ChessPosition -> (ChessBoard, Maybe ChessPiece)
    overwriteMaybe (ChessBoardSquare (Just piece) srcPos) destPos =
      ( writeSquares b [ChessBoardSquare (Just piece) destPos, ChessBoardSquare Nothing srcPos],
        squarePiece $ squareAt b destPos
      )
    overwriteMaybe _ _ = (b, Nothing)

instance Show ChessBoard where
  show (ChessBoard v) =
    intercalate
      "\n"
      $ (fmap unwords . chunksOf maxSquares . fmap show . V.toList)
        v
