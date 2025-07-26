{-# LANGUAGE InstanceSigs #-}

module Chess.Move
  ( ChessMove (..),
    ChessPosition (..),
    DisambPosition,
    disambMatch,
    parseMove,
  )
where

import Chess.Terminology
  ( ChessColor,
    ChessFile (..),
    ChessPiece (ChessPiece),
    ChessPieceType (..),
    ChessRank (..),
  )
import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text.Lazy as LT
import Data.Functor (($>))
import Data.Text (pack)

data ChessPosition = ChessPosition ChessFile ChessRank

instance Show ChessPosition where
  show :: ChessPosition -> String
  show (ChessPosition file rank) = show file ++ show rank

data DisambPosition = Disamb
  { disambFile :: Maybe ChessFile,
    disambRank :: Maybe ChessRank
  }

disambMatch :: DisambPosition -> ChessPosition -> Bool
disambMatch (Disamb Nothing Nothing) _ = False
disambMatch (Disamb (Just file) (Just rank)) (ChessPosition f r) = file == f && rank == r
disambMatch (Disamb (Just file) Nothing) (ChessPosition f _) = file == f
disambMatch (Disamb Nothing (Just rank)) (ChessPosition _ r) = rank == r

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
        captureSrc :: DisambPosition,
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
    <|> ( PieceCapture
            <$> ( ChessPiece color <$> parsePieceType
                )
            <*> parseDisamb
            <*> (parseCaptures *> parseChessPosition)
            <*> return MoveUnrestricted
        )
    <* LT.endOfInput

parseDisamb :: LT.Parser DisambPosition
parseDisamb =
  Disamb
    <$> (Just <$> parseChessFile <|> return Nothing)
    <*> (Just <$> parseChessRank <|> return Nothing)

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