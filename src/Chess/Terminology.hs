{-# LANGUAGE InstanceSigs #-}

module Chess.Terminology
  ( ChessFile (..),
    ChessPiece (..),
    ChessPieceType (..),
    ChessRank (..),
    PieceGenerator,
    -- Colors
    ChessColor (..),
  )
where

data ChessColor = ChessBlack | ChessWhite deriving (Eq)

instance Show ChessColor where
  show ChessBlack = "black"
  show ChessWhite = "white"

data ChessPieceType
  = ChessBishop
  | ChessKing
  | ChessKnight
  | ChessPawn
  | ChessRook
  | ChessQueen
  deriving (Eq)

data ChessRank
  = R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  deriving (Eq)

instance Show ChessRank where
  show :: ChessRank -> String
  show R1 = "1"
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"

data ChessFile
  = FA
  | FB
  | FC
  | FD
  | FE
  | FF
  | FG
  | FH
  deriving (Eq)

instance Show ChessFile where
  show FA = "a"
  show FB = "b"
  show FC = "c"
  show FD = "d"
  show FE = "e"
  show FF = "f"
  show FG = "g"
  show FH = "h"

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

instance Bounded ChessFile where
  minBound :: ChessFile
  minBound = FA
  maxBound :: ChessFile
  maxBound = FH

instance Bounded ChessRank where
  minBound :: ChessRank
  minBound = R1
  maxBound :: ChessRank
  maxBound = R8

data ChessPiece = ChessPiece
  { pieceColor :: ChessColor,
    pieceType :: ChessPieceType
  }
  deriving (Eq)

fromNotation :: Char -> Maybe PieceGenerator
fromNotation 'B' = Just bishop
fromNotation 'K' = Just king
fromNotation 'N' = Just knight
fromNotation 'Q' = Just queen
fromNotation 'R' = Just rook
fromNotation _ = Nothing

instance Show ChessPiece where
  show (ChessPiece ChessWhite ChessBishop) = ['♗']
  show (ChessPiece ChessBlack ChessBishop) = ['♝']
  show (ChessPiece ChessWhite ChessKnight) = ['♘']
  show (ChessPiece ChessBlack ChessKnight) = ['♞']
  show (ChessPiece ChessWhite ChessKing) = ['♔']
  show (ChessPiece ChessBlack ChessKing) = ['♚']
  show (ChessPiece ChessWhite ChessPawn) = ['♙']
  show (ChessPiece ChessBlack ChessPawn) = ['♟']
  show (ChessPiece ChessWhite ChessQueen) = ['♕']
  show (ChessPiece ChessBlack ChessQueen) = ['♛']
  show (ChessPiece ChessWhite ChessRook) = ['♖']
  show (ChessPiece ChessBlack ChessRook) = ['♜']

-- TODO: HSdocs
type PieceGenerator = ChessColor -> ChessPiece

black :: ChessColoring
black gen = gen ChessBlack

white :: ChessColoring
white gen = gen ChessWhite

-- TODO: HSdocs
type ChessColoring = PieceGenerator -> ChessPiece

coloring :: ChessColor -> ChessColoring
coloring ChessBlack = black
coloring ChessWhite = white

bishop :: PieceGenerator
bishop ChessWhite = ChessPiece ChessWhite ChessBishop
bishop ChessBlack = ChessPiece ChessWhite ChessBishop

knight :: PieceGenerator
knight ChessWhite = ChessPiece ChessWhite ChessKnight
knight ChessBlack = ChessPiece ChessBlack ChessKnight

king :: PieceGenerator
king ChessWhite = ChessPiece ChessWhite ChessKing
king ChessBlack = ChessPiece ChessBlack ChessKing

pawn :: PieceGenerator
pawn ChessWhite = ChessPiece ChessWhite ChessPawn
pawn ChessBlack = ChessPiece ChessBlack ChessPawn

queen :: PieceGenerator
queen ChessWhite = ChessPiece ChessWhite ChessQueen
queen ChessBlack = ChessPiece ChessBlack ChessQueen

rook :: PieceGenerator
rook ChessWhite = ChessPiece ChessWhite ChessRook
rook ChessBlack = ChessPiece ChessBlack ChessRook