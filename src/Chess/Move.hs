{-# LANGUAGE InstanceSigs #-}

module Chess.Move
  ( ChessMove (..),
    ChessPosition (..),
    DisambPosition,
    disambMatch,
    moveRestriction,
    parseMove,
    stateFromRestriction,
  )
where

import Chess.Terminology
  ( ChessColor,
    ChessFile (..),
    ChessGameState (..),
    ChessPiece (ChessPiece),
    ChessPieceType (..),
    ChessRank (..),
    enemyColor,
  )
import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text.Lazy as LT
import Data.Functor (($>))
import Data.Text (pack)

data ChessPosition = ChessPosition ChessFile ChessRank

instance Bounded ChessPosition where
  minBound :: ChessPosition
  minBound = ChessPosition (minBound :: ChessFile) (minBound :: ChessRank)

  maxBound :: ChessPosition
  maxBound = ChessPosition (maxBound :: ChessFile) (maxBound :: ChessRank)

instance Enum ChessPosition where
  fromEnum :: ChessPosition -> Int
  fromEnum (ChessPosition file rank) = (fromEnum file - 1) * fromEnum (maxBound :: ChessRank) + (fromEnum rank - 1)

  toEnum :: Int -> ChessPosition
  toEnum i
    | i >= fromEnum (maxBound :: ChessPosition) = maxBound
    | i <= fromEnum (minBound :: ChessPosition) = minBound
    | otherwise = ChessPosition file rank
    where
      file = toEnum $ (i `mod` delta) + 1
      rank = toEnum $ (i `div` delta) + 1
      delta = fromEnum (maxBound :: ChessRank)

instance Show ChessPosition where
  show :: ChessPosition -> String
  show (ChessPosition file rank) = show file ++ show rank

data DisambPosition = Disamb
  { disambFile :: Maybe ChessFile,
    disambRank :: Maybe ChessRank
  }
  deriving (Show)

disambMatch :: DisambPosition -> ChessPosition -> Bool
disambMatch (Disamb Nothing Nothing) _ = False
disambMatch (Disamb (Just file) (Just rank)) (ChessPosition f r) = file == f && rank == r
disambMatch (Disamb (Just file) Nothing) (ChessPosition f _) = file == f
disambMatch (Disamb Nothing (Just rank)) (ChessPosition _ r) = rank == r

data ChessMoveRestriction = MoveUnrestricted | CheckRestricted | Checkmate deriving (Show)

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
  | QueensideCastle ChessColor ChessMoveRestriction
  | KingsideCastle ChessColor ChessMoveRestriction
  deriving (Show)

-- TODO: Parsec parse is the following
-- TODO: Add move restrictions to castling (for those rare cases)
-- [Piece][File/Rank][Captures]FileRank[Restriction]
parseMove :: ChessColor -> String -> Either String ChessMove
parseMove color "O-O" = Right $ KingsideCastle color MoveUnrestricted
parseMove color "O-O-O" = Right $ QueensideCastle color MoveUnrestricted
parseMove color s = LT.parseOnly (parseChessMove color) (pack s)

moveRestriction :: ChessMove -> ChessMoveRestriction
moveRestriction p@(PieceMove _ _ r) = r
moveRestriction p@(PieceCapture _ _ _ r) = r
moveRestriction (QueensideCastle _ r) = r
moveRestriction (KingsideCastle _ r) = r

colorFrom :: ChessMove -> ChessColor
colorFrom (PieceMove (ChessPiece color _) _ _) = color
colorFrom (PieceCapture (ChessPiece color _) _ _ _) = color
colorFrom (KingsideCastle color _) = color
colorFrom (QueensideCastle color _) = color

stateFromRestriction :: ChessMove -> Maybe ChessGameState
stateFromRestriction move = fmap ($ color) $ stateFrom' $ moveRestriction move
  where
    -- The move itself takes on the attacker's color, but the state is defined
    -- on who's in check or checkmate. Thus we take the enemy's color
    color = enemyColor $ colorFrom move
    stateFrom' CheckRestricted = Just ChessCheck
    stateFrom' Checkmate = Just ChessCheckmate
    stateFrom' _ = Nothing

-- TODO: Require pawn captures to specify file at least
parseChessMove ::
  ChessColor ->
  LT.Parser ChessMove
parseChessMove color =
  ( PieceMove <$> (ChessPiece color <$> parsePieceType)
      <*> parseChessPosition
      <*> (parseMoveRestriction <|> return MoveUnrestricted)
  )
    <|> ( PieceCapture
            <$> ( ChessPiece color <$> parsePieceType
                )
            <*> parseDisamb
            <*> (parseCaptures *> parseChessPosition)
            <*> (parseMoveRestriction <|> return MoveUnrestricted)
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

parseMoveRestriction :: LT.Parser ChessMoveRestriction
parseMoveRestriction =
  LT.choice
    [ LT.char '+' $> CheckRestricted,
      LT.char '#' $> Checkmate
    ]