module Chess.Rules
    ( moves
    ) where

import Chess.Types.Board
import Chess.Types.Unit
import Data.Ix (range)

moves :: Board -> ChessCoord -> Maybe [ChessCoord]
moves b c = do (Piece _ pieceClass) <- pieceAt b c
               return $ pieceMove pieceClass b c

pieceMove :: PieceClass -> Board -> ChessCoord -> [ChessCoord]
pieceMove King b s = filter (inBounds b) $ ((uncurry ChessCoord) . ff) <$> (filter (/= (0,0)) $ range ((-1, -1), (1,1)))
    where f = (+ (boardRank s)) . Rank
          f' = (+ (boardFile s)) . File
          ff (rank, file) = (f rank, f' file)

pieceMove Rook b s = up ++ down ++ left ++ right
    where takeValidRiderSquares ri fi sq = (takeWhile (inBounds b)) $ riderSquares ri fi sq
          up    = takeValidRiderSquares 1 0 s
          down  = takeValidRiderSquares (-1) 0 s
          right = takeValidRiderSquares 0 1 s
          left  = takeValidRiderSquares 0 (-1) s

pieceMove Bishop b s = ne ++ nw ++ se++ sw
    where takeValidRiderSquares ri fi sq = (takeWhile (inBounds b)) $ riderSquares ri fi sq
          ne = takeValidRiderSquares 1 1 s
          nw = takeValidRiderSquares 1 (-1) s
          se = takeValidRiderSquares (-1) 1 s
          sw = takeValidRiderSquares (-1) (-1) s

pieceMove Queen b s = (pieceMove Rook b s) ++ (pieceMove Bishop b s)

pieceMove Knight b s = filter (inBounds b) $ newSq <$> mvmts
    where f op (a,b) (c,d) = (op a c, op b d)
          mvmts = (f (*)) <$> [(-1,-1), (-1,1), (1,-1), (1,1)] <*> [(1,2), (2,1)]
          newSq (a, b) = squareUp s (+ Rank b) (+ File a)

pieceMove Pawn b s = [rankUp s ((1 :: Rank) +)]
