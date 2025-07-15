module Chess.Move (ChessMove (..), ChessPosition (..), parseMove) where

import Chess.Terminology
import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text.Lazy as LT
import Data.Functor (($>))
import Data.Text (pack)

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

-- TODO: Parsec parse is the following
-- [Piece][File/Rank][Captures]FileRank[Restriction]
parseMove :: ChessColor -> String -> Either String ChessMove
parseMove _ "O-O" = Right KingsideCastle
parseMove _ "O-O-O" = Right QueensideCastle
parseMove color s = LT.parseOnly (parseChessMove color) (pack s)

-- TODO: Require pawn captures to specify file at least
parseChessMove ::
  ChessColor ->
  LT.Parser ChessMove
parseChessMove color =
  ( PieceMove <$> (ChessPiece color <$> parsePieceType)
      <*> parseChessPosition
      <*> return MoveUnrestricted
  )
    <|> (PieceCapture (ChessPiece color ChessPawn) <$> (parseCaptures *> parseChessPosition) <*> return MoveUnrestricted)
    <* LT.endOfInput

parsePieceType :: LT.Parser ChessPieceType
parsePieceType =
  LT.choice
    ( mapPiece
        <$> [ ('B', ChessBishop),
              ('N', ChessKnight),
              ('K', ChessKing),
              ('R', ChessRook),
              ('Q', ChessQueen)
            ]
    )
    <|> return ChessPawn
  where
    mapPiece :: (Char, ChessPieceType) -> LT.Parser ChessPieceType
    mapPiece (c, p) = LT.char c $> p

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