module Chess.Rules.Moves
    (

    ) where

import Chess.Rules.ChessBoard
    ( ChessBoard (..)
    , ChessCoord (..)
    , onSquare
    , tformCoord
    )
import Chess.Rules.Unit
import Data.Bifunctor
import Data.Ix (range)

toFirstCapture :: ChessBoard -> ChessColor -> [ChessCoord] -> [ChessCoord]
toFirstCapture b color (c:cs) = untilFirstCapture $ b `onSquare` c
    where oppColor = opposingChessColor color
          untilFirstCapture (Just (ChessPiece oppColor _)) = [c]
          untilFirstCapture (Just Open)                    = c : toFirstCapture b color cs
          untilFirstCapture _                              = []

riderMvmts :: Int -> Int -> ChessCoord -> [ChessCoord]
riderMvmts ri fi origin = drop 1 $ zipWith ChessCoord ranks files
    where sr = boardRank origin
          sf = boardFile origin
          ranks = [sr, sr + ri]
          files = [sf, sf + fi]

hopperMvmts :: [ChessCoord] -> ChessCoord -> [ChessCoord]
hopperMvmts incs (ChessCoord sr sf) = (tformCoord (sr+) (sf+)) <$> incs

-- pieceMoves :: ChessPieceClass -> Board -> ChessCoord -> [ChessCoord]
-- pieceMoves
-- pieceMoves _ King = hopperMvmts kingOffsets
--     where kingOffsets = filter (/= (0,0)) $ range ((-1, -1), (1,1))

-- pieceMoves _ Knight coord = rankHigh ++ fileHigh
--     where baseMvmts = [(-1,-1), (-1,1), (1,-1), (1,1)]
--           rankHigh = hopperMvmts ((bimap (2*) id) <$> baseMvmts) coord
--           fileHigh = hopperMvmts ((bimap id (2*)) <$> baseMvmts) coord

-- pieceMoves _ Rook =

-- pieceMoves _ Queen origin = (pieceMoves Rook origin) ++ (pieceMoves Bishop origin)

----
-- pieceMove :: ChessPiece -> ChessBoard -> ChessCoord -> [ChessCoord]
-- pieceMove (ChessPiece _ King) b s = filter (validChessCoord b) $ toChessCoords <$> kingMvmts
--     where kingOffsets = filter (/= (0,0)) $ range ((-1, -1), (1,1))
--           kingMvmts = hopperMvmts kingOffsets (toIntCoords s)

-- pieceMove (ChessPiece _ Rook) b s = up ++ down ++ left ++ right
--     where takeValidRiderSquares ri fi sq = takeWhile (validChessCoord b) $ riderSquares ri fi sq
--           up    = takeValidRiderSquares 1 0 s
--           down  = takeValidRiderSquares (-1) 0 s
--           right = takeValidRiderSquares 0 1 s
--           left  = takeValidRiderSquares 0 (-1) s

-- pieceMove (ChessPiece _ Bishop) b s = ne ++ nw ++ se ++ sw
--     where takeValidRiderSquares ri fi sq = takeWhile (validChessCoord b) $ riderSquares ri fi sq
--           ne = takeValidRiderSquares 1 1 s
--           nw = takeValidRiderSquares 1 (-1) s
--           se = takeValidRiderSquares (-1) 1 s
--           sw = takeValidRiderSquares (-1) (-1) s

-- pieceMove (ChessPiece color Queen) b c = rookMoves ++ bishopMoves
--     where rookMoves = pieceMove (ChessPiece color Rook) b c
--           bishopMoves = pieceMove (ChessPiece color Bishop) b c

-- pieceMove (ChessPiece _ Knight) b s = toChessCoords <$> knightSquares
--     where baseMvmts = [(-1,-1), (-1,1), (1,-1), (1,1)]
--           origin = toIntCoords s
--           rankHigh = hopperMvmts ((bimap (2*) id) <$> baseMvmts) origin
--           fileHigh = hopperMvmts ((bimap id (2*)) <$> baseMvmts) origin
--           knightMvmts = hopperMvmts (rankHigh ++ fileHigh)


--           knightSquares = filter (inBounds $ chessBoard b) $ (fromStart s) <$> mvmts

-- pieceMove (ChessPiece color Pawn) b c = [toChessCoords (rank+1, file)]
--     where (rank, file) = toIntCoords c
