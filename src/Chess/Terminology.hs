{-# LANGUAGE InstanceSigs #-}

module Chess.Terminology
  ( ChessPiece (..),
    ChessFile (..),
    ChessRank (..),
    PieceGenerator,
    -- Colors
    ChessColor (..),
    ChessColoring,
    black,
    coloring,
    white,
    -- Units
    bishop,
    king,
    knight,
    pawn,
    queen,
    rook,
  )
where

data ChessColor = ChessBlack | ChessWhite

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

newtype ChessPiece = ChessPiece Char deriving (Eq)

fromNotation :: Char -> Maybe PieceGenerator
fromNotation 'B' = Just bishop
fromNotation 'K' = Just king
fromNotation 'N' = Just knight
fromNotation 'Q' = Just queen
fromNotation 'R' = Just rook
fromNotation _ = Nothing

instance Show ChessPiece where
  show (ChessPiece p) = [p]

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
