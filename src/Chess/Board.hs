module Chess.Board
  ( ChessBoard,
    chessBoard,
    squareAt,
  )
where

import Chess.Move (ChessPosition (..))
import Chess.Terminology
  ( ChessColor (..),
    ChessColoring,
    ChessFile (..),
    ChessPiece,
    ChessRank (..),
    PieceGenerator,
    bishop,
    black,
    coloring,
    king,
    knight,
    pawn,
    queen,
    rook,
    white,
  )
import Data.List (intercalate)
import qualified Data.Vector as V

newtype ChessBoardSquare = ChessBoardSquare (Maybe ChessPiece)

instance Show ChessBoardSquare where
  show (ChessBoardSquare (Just piece)) = show piece
  show (ChessBoardSquare Nothing) = "_"

newtype ChessBoard = ChessBoard (V.Vector (V.Vector ChessBoardSquare))

startingPromotedRow :: [PieceGenerator]
startingPromotedRow = [rook, knight, bishop, queen, king, bishop, knight, rook]

startingPawnRow :: [PieceGenerator]
startingPawnRow = replicate 8 pawn

chessBoard :: ChessBoard
chessBoard = ChessBoard (V.fromList $ V.fromList <$> rawBoard)
  where
    rawBoard =
      [ ChessBoardSquare . Just . black <$> startingPromotedRow,
        ChessBoardSquare . Just . black <$> startingPawnRow,
        blankRow,
        blankRow,
        blankRow,
        blankRow,
        ChessBoardSquare . Just . white <$> startingPawnRow,
        ChessBoardSquare . Just . white <$> startingPromotedRow
      ]

    blankRow = replicate 8 (ChessBoardSquare Nothing)

-- TODO Test this
squareAt :: ChessBoard -> ChessPosition -> ChessBoardSquare
squareAt (ChessBoard v) (ChessPosition file rank) = (V.!) ((V.!) v rank') file'
  where
    file' = fromEnum file - 1 -- Offset by 1 since enums are 1-indexed
    rank' = 8 - fromEnum rank -- Subtract from 8 since ranks are stored in reverse

instance Show ChessBoard where
  show (ChessBoard v) =
    intercalate
      "\n"
      ( unwords . fmap show . V.toList
          <$> V.toList v
      )
