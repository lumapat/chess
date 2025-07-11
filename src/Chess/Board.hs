{-# LANGUAGE InstanceSigs #-}

module Chess.Board
  ( ChessPosition (..),
    ChessRank (..),
    ChessFile (..),
    -- Board
    chessBoard,
  )
where

import Data.List (intercalate)
import qualified Data.Vector as V

data ChessRank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Eq, Show)

data ChessFile = FA | FB | FC | FD | FE | FF | FG | FH deriving (Eq, Show)

instance Enum ChessRank where
  fromEnum R1 = 1
  fromEnum R2 = 2
  fromEnum R3 = 3
  fromEnum R4 = 4
  fromEnum R5 = 5
  fromEnum R6 = 6
  fromEnum R7 = 7
  fromEnum R8 = 8

  toEnum :: Int -> ChessRank
  toEnum 1 = R1
  toEnum 2 = R2
  toEnum 3 = R3
  toEnum 4 = R4
  toEnum 5 = R5
  toEnum 6 = R6
  toEnum 7 = R7
  toEnum _ = R8

instance Enum ChessFile where
  fromEnum FA = 1
  fromEnum FB = 2
  fromEnum FC = 3
  fromEnum FD = 4
  fromEnum FE = 5
  fromEnum FF = 6
  fromEnum FG = 7
  fromEnum FH = 8

  toEnum :: Int -> ChessFile
  toEnum 1 = FA
  toEnum 2 = FB
  toEnum 3 = FC
  toEnum 4 = FD
  toEnum 5 = FE
  toEnum 6 = FF
  toEnum 7 = FG
  toEnum _ = FH

data ChessPosition = ChessPosition ChessFile ChessRank

newtype ChessPiece = ChessPiece Char

instance Show ChessPiece where
  show (ChessPiece p) = [p]

newtype ChessBoardSquare = ChessBoardSquare (Maybe ChessPiece)

instance Show ChessBoardSquare where
  show (ChessBoardSquare (Just piece)) = show piece
  show (ChessBoardSquare Nothing) = "_"

data ChessColor = ChessBlack | ChessWhite

type PieceGenerator = ChessColor -> ChessPiece

black :: PieceGenerator -> ChessPiece
black gen = gen ChessBlack

white :: PieceGenerator -> ChessPiece
white gen = gen ChessWhite

bishop :: PieceGenerator
bishop ChessWhite = ChessPiece '♗'
bishop ChessBlack = ChessPiece '♝'

knight :: PieceGenerator
knight ChessWhite = ChessPiece '♘'
knight ChessBlack = ChessPiece '♞'

king :: PieceGenerator
king ChessWhite = ChessPiece '♔'
king ChessBlack = ChessPiece '♚'

pawn :: PieceGenerator
pawn ChessWhite = ChessPiece '♙'
pawn ChessBlack = ChessPiece '♟'

queen :: PieceGenerator
queen ChessWhite = ChessPiece '♕'
queen ChessBlack = ChessPiece '♛'

rook :: PieceGenerator
rook ChessWhite = ChessPiece '♖'
rook ChessBlack = ChessPiece '♜'

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
        blankRow,
        blankRow,
        blankRow,
        blankRow,
        blankRow,
        blankRow,
        ChessBoardSquare . Just . white <$> startingPromotedRow
      ]

    blankRow = replicate 8 (ChessBoardSquare Nothing)

instance Show ChessBoard where
  show (ChessBoard v) =
    intercalate
      "\n"
      ( unwords . fmap show . V.toList
          <$> V.toList v
      )