module Chess.Rules.ChessBoard
    ( ChessBoard (..)
    , ChessCoord (..)
    , inBoard
    , onSquare
    , pawnStartingRank
    , startingBoard
    , tformCoord
    ) where

import Chess.Rules.Unit
import Types.Board

data ChessBoard = ChessBoard
    { chessBoard :: Board ChessPiece
    } deriving (Eq, Show)

data ChessCoord = ChessCoord
    { boardRank :: Int
    , boardFile :: Int
    } deriving (Eq, Ord, Show)

tformCoord :: (Int -> Int) -> (Int -> Int) -> ChessCoord -> ChessCoord
tformCoord f g (ChessCoord r c) = ChessCoord (f r) (g c)

toBoardCoord :: ChessCoord -> BoardCoord
toBoardCoord (ChessCoord r f) = BoardCoord r f

onSquare :: ChessBoard -> ChessCoord -> Maybe ChessPiece
onSquare b coord = (chessBoard b) Types.Board.!? (toBoardCoord coord)

maxRanks = 8
maxFiles = 8

-- TODO: Codify this contract
pawnStartingRank :: ChessColor -> Int
pawnStartingRank Black = 6
pawnStartingRank White = 1

-- Coordinates start from top-right
startingBoard :: ChessBoard
startingBoard = ChessBoard $ newBoard maxRanks maxFiles boardSetup Open
    where alternatingColorsOf = zipWith alternateColoredRow (cycle [White, Black])
          boardSetup = [ royalRow White
                       , peasantRow White
                       , blankRow
                       , blankRow
                       , blankRow
                       , blankRow
                       , peasantRow Black
                       , royalRow Black
                       ]

inBoard :: ChessBoard -> ChessCoord -> Bool
inBoard b c = inBounds (chessBoard b) (toBoardCoord c)

alternateColoredRow :: ChessColor -> [ChessPieceClass] -> [ChessPiece]
alternateColoredRow color pieces = ChessPiece <$> colors <*> pieces
    where colors = alternatingColors <$> [1..]
          alternatingColors ix | even ix   = color
                               | otherwise = opposingChessColor color

blankRow :: [ChessPiece]
blankRow = replicate maxFiles Open

peasantRow :: ChessColor -> [ChessPiece]
peasantRow color = replicate maxFiles (ChessPiece color Pawn)

royalRow :: ChessColor -> [ChessPiece]
royalRow color = (ChessPiece color) <$> [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
