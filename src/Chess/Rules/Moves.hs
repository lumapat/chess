module Chess.Rules.Moves
    (

    ) where

import Chess.Rules.ChessBoard
    ( ChessBoard (..)
    , ChessCoord (..)
    , inBoard
    , onSquare
    , tformCoord
    )
import Chess.Rules.Unit
import Data.Bifunctor
import Data.Ix (range)

data ChessMove = Move ChessPiece ChessCoord
               | Capture ChessPiece ChessCoord
               | KingsideCastle ChessColor
               | QueensideCastle ChessColor

canCapture :: ChessBoard -> ChessPiece -> ChessCoord -> Bool
canCapture b piece coord = isOpposing == Just True
    where isOpposing = do targetPiece <- b `onSquare` coord
                          return $ opposingChessPieces piece targetPiece

canMove :: ChessBoard -> ChessCoord -> Bool
canMove b coord = b `onSquare` coord == Just Open

moveOf :: ChessBoard -> ChessPiece -> ChessCoord -> Maybe ChessMove
moveOf b piece coord | canCapture b piece coord = Just $ Capture piece coord
                     | canMove b coord          = Just $ Move piece coord
                     | otherwise                = Nothing

toFirstCapture :: ChessBoard -> ChessPiece -> [ChessCoord] -> [ChessMove]
toFirstCapture b piece (c:cs) | canMove b c = (Move piece c) : toFirstCapture b piece cs
                              | canCapture b piece c = [Capture piece c]
                              | otherwise = []

takeAllMoves :: ChessBoard -> ChessPiece -> [ChessCoord] -> [ChessMove]
takeAllMoves b piece coords = onlyValid $ (moveOf b piece) <$> coords
    where onlyValid ((Just c) : cs) = c : onlyValid cs
          onlyValid _               = []

riderMvmts :: Int -> Int -> ChessCoord -> [ChessCoord]
riderMvmts ri fi origin = drop 1 $ zipWith ChessCoord ranks files
    where sr = boardRank origin
          sf = boardFile origin
          ranks = [sr, sr + ri]
          files = [sf, sf + fi]

hopperMvmts :: [ChessCoord] -> ChessCoord -> [ChessCoord]
hopperMvmts incs (ChessCoord sr sf) = (tformCoord (sr+) (sf+)) <$> incs

moves :: ChessBoard
      -> ChessCoord
      -> [ChessMove]
moves board origin = movesIfValid $ board `onSquare` origin
    where movesIfValid Nothing = []
          movesIfValid (Just p) = pieceMoves p

          pieceMoves :: ChessPiece -> [ChessMove]
          pieceMoves piece@(ChessPiece _ King) = takeAllMoves board piece $ hopperMvmts (toCoord <$> kingOffsets) origin
            where kingOffsets = filter (/= (0,0)) $ range ((-1, -1), (1,1))
                  toCoord (a,b) = ChessCoord a b

          pieceMoves piece@(ChessPiece _ Knight) = takeAllMoves board piece $ rankHigh ++ fileHigh
            where baseMvmts = [(-1,-1), (-1,1), (1,-1), (1,1)]
                  toCoord (a,b) = ChessCoord a b
                  rankHigh = hopperMvmts ((toCoord . (bimap (2*) id)) <$> baseMvmts) origin
                  fileHigh = hopperMvmts ((toCoord . (bimap id (2*))) <$> baseMvmts) origin

          pieceMoves piece@(ChessPiece color Bishop) = mconcat $ (toFirstCapture board piece) <$> [ne, nw, se, sw]
            where ne = riderMvmts 1 1 origin
                  nw = riderMvmts 1 (-1) origin
                  se = riderMvmts (-1) 1 origin
                  sw = riderMvmts (-1) (-1) origin

          pieceMoves piece@(ChessPiece color Rook) = mconcat $ (toFirstCapture board piece) <$> [up, down, left, right]
            where up    = riderMvmts 1 0 origin
                  down  = riderMvmts (-1) 0 origin
                  left  = riderMvmts 0 (-1) origin
                  right = riderMvmts 0 1 origin

          pieceMoves (ChessPiece color Queen) = rookMoves ++ bishopMoves
            where rookMoves = pieceMoves (ChessPiece color Rook)
                  bishopMoves = pieceMoves (ChessPiece color Bishop)

          -- TODO: Actually implement this correctly
          pieceMoves (ChessPiece _ Pawn) = []