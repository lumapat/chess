module Chess.Game
    (
    ) where

import           Chess.Rules.ChessBoard
import           Chess.Rules.Unit

data GameState = Play ChessColor
               | Check ChessColor
               | Win ChessColor
               | Draw

data Action = Move ChessCoord ChessCoord
            | KingsideCastle
            | QueensideCastle
            | Forfeit

data Game = Game
    { gameState :: GameState
    , gameBoard :: ChessBoard
    }

newGame :: Game
newGame = Game
    { gameState = Play White
    , gameBoard = startingBoard
    }

-- Continue to play until we have a check condition
--      Player must make a move out of check or forfeit
-- Game stops when a draw or win occurs
advance :: Action -> Game -> Game
advance _ g = g
