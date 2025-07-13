{-# LANGUAGE InstanceSigs #-}

module Chess.Board
  ( ChessBoard,
    ChessColor (..),
    ChessColoring,
    ChessFile (..),
    ChessMove (..),
    ChessPosition (..),
    ChessRank (..),
    coloring,
    -- Board
    chessBoard,
    -- Moves
    parseMove,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text.Lazy as LT
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Text (pack)
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

data ChessMoveRestriction = MoveUnrestricted | CheckRestricted | Checkmate

-- TODO: Add non-pawn chess pieces
-- TODO: Disambiguating moves
-- TODO: Promotions
-- TODO: En passant
data ChessMove
  = PieceMove
      { movingPiece :: ChessPiece,
        dest :: ChessPosition,
        restriction :: ChessMoveRestriction
      }
  | PieceCapture
      { capturingPiece :: ChessPiece,
        dest :: ChessPosition,
        restriction :: ChessMoveRestriction
      }
  | QueensideCastle
  | KingsideCastle

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

newtype ChessBoardSquare = ChessBoardSquare (Maybe ChessPiece)

instance Show ChessBoardSquare where
  show (ChessBoardSquare (Just piece)) = show piece
  show (ChessBoardSquare Nothing) = "_"

data ChessColor = ChessBlack | ChessWhite

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
squareAt (ChessBoard v) (ChessPosition file rank) = (V.!) ((V.!) v file') rank'
  where
    file' = 8 - fromEnum file
    rank' = 8 - fromEnum rank

instance Show ChessBoard where
  show (ChessBoard v) =
    intercalate
      "\n"
      ( unwords . fmap show . V.toList
          <$> V.toList v
      )

-- TODO: Parsec parse is the following
-- [Piece][File/Rank][Captures]FileRank[Restriction]
parseMove :: ChessColoring -> String -> Either String ChessMove
parseMove _ "O-O" = Right KingsideCastle
parseMove _ "O-O-O" = Right QueensideCastle
parseMove color s = LT.parseOnly (parseChessMove color) (pack s)

parseChessMove ::
  (PieceGenerator -> ChessPiece) ->
  LT.Parser ChessMove
parseChessMove color =
  ( PieceMove (color pawn)
      <$> parseChessPosition
      <*> return MoveUnrestricted
  )
    <|> (PieceCapture (color pawn) <$> (parseCaptures *> parseChessPosition) <*> return MoveUnrestricted)
    <* LT.endOfInput

parseCaptures :: LT.Parser Char
parseCaptures = LT.char 'x'

parseChessPosition :: LT.Parser ChessPosition
parseChessPosition = ChessPosition <$> parseChessFile <*> parseChessRank

parseChessRank :: LT.Parser ChessRank
parseChessRank = LT.choice ranks
  where
    ranks =
      [ LT.char '1' $> R1,
        LT.char '2' $> R2,
        LT.char '3' $> R3,
        LT.char '4' $> R4,
        LT.char '5' $> R5,
        LT.char '6' $> R6,
        LT.char '7' $> R7,
        LT.char '8' $> R8
      ]

parseChessFile :: LT.Parser ChessFile
parseChessFile = LT.choice files
  where
    files =
      [ LT.char 'a' $> FA,
        LT.char 'b' $> FB,
        LT.char 'c' $> FC,
        LT.char 'd' $> FD,
        LT.char 'e' $> FE,
        LT.char 'f' $> FF,
        LT.char 'g' $> FG,
        LT.char 'h' $> FH
      ]