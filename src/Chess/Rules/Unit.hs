module Chess.Rules.Unit
    ( ChessColor (..)
    , ChessPiece (..)
    , ChessPieceClass(..)
    , opposingChessColor
    , opposingChessPieces
    ) where

data ChessColor = White
                | Black
                deriving (Enum, Eq, Ord, Show)

opposingChessColor :: ChessColor -> ChessColor
opposingChessColor Black = White
opposingChessColor White = Black

data ChessPieceClass = King
                     | Queen
                     | Rook
                     | Bishop
                     | Knight
                     | Pawn
                     deriving (Eq, Show)

-- TODO: Use Maybe instead
data ChessPiece = ChessPiece ChessColor ChessPieceClass
                | Open
                deriving (Eq)

opposingChessPieces :: ChessPiece -> ChessPiece -> Bool
opposingChessPieces (ChessPiece c1 _) (ChessPiece c2 _) = True
opposingChessPieces _ _                                 = False

instance Show ChessPiece where
    show Open = " "

    show (ChessPiece Black King) = "♚"
    show (ChessPiece Black Queen) = "♛"
    show (ChessPiece Black Rook) = "♜"
    show (ChessPiece Black Bishop) = "♝"
    show (ChessPiece Black Knight) = "♞"
    show (ChessPiece Black Pawn) = "♟"

    show (ChessPiece White King) = "♔"
    show (ChessPiece White Queen) = "♕"
    show (ChessPiece White Rook) = "♖"
    show (ChessPiece White Bishop) = "♗"
    show (ChessPiece White Knight) = "♘"
    show (ChessPiece White Pawn) = "♙"

instance Enum ChessPieceClass where
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
