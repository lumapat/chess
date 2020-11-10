module Chess.Types.Unit
    ( ChessColor (..)
    , Piece (..)
    , PieceClass(..)
    , opposingChessColor
    ) where

data ChessColor = White
                | Black
                deriving (Enum, Eq, Ord, Show)

opposingChessColor :: ChessColor -> ChessColor
opposingChessColor Black = White
opposingChessColor White = Black

-- TODO: Rename to ChessPieceClass
data PieceClass = King
                | Queen
                | Rook
                | Bishop
                | Knight
                | Pawn
                deriving (Eq, Show)

-- TODO: Rename to ChessPiece
data Piece = Piece ChessColor PieceClass
           | Open
           deriving (Eq)

instance Show Piece where
    show Open = " "

    show (Piece Black King) = "♚"
    show (Piece Black Queen) = "♛"
    show (Piece Black Rook) = "♜"
    show (Piece Black Bishop) = "♝"
    show (Piece Black Knight) = "♞"
    show (Piece Black Pawn) = "♟"

    show (Piece White King) = "♔"
    show (Piece White Queen) = "♕"
    show (Piece White Rook) = "♖"
    show (Piece White Bishop) = "♗"
    show (Piece White Knight) = "♘"
    show (Piece White Pawn) = "♙"

instance Enum PieceClass where
    fromEnum King = 1
    fromEnum Queen = 2
    fromEnum Rook = 3
    fromEnum Bishop = 4
    fromEnum Knight = 5
    fromEnum Pawn = 6

    toEnum 1 = King
    toEnum 2 = Queen
    toEnum 3 = Rook
    toEnum 4 = Bishop
    toEnum 5 = Knight
    toEnum 6 = Pawn

