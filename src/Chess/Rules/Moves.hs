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

-- TODO: Identify Capture vs Move

takeWhileInBoard :: ChessBoard -> [ChessCoord] -> [ChessCoord]
takeWhileInBoard b = takeWhile (inBoard b)

toFirstCapture :: ChessBoard -> ChessColor -> [ChessCoord] -> [ChessCoord]
toFirstCapture b color (c:cs) = untilFirstCapture $ b `onSquare` c
    where oppColor = opposingChessColor color
          untilFirstCapture (Just (ChessPiece oppColor _)) = [c]
          untilFirstCapture (Just Open)                    = c : toFirstCapture b color cs
          untilFirstCapture _                              = []

takeToFirstCapture :: ChessBoard -> ChessColor -> [ChessCoord] -> [ChessCoord]
takeToFirstCapture b c = (toFirstCapture b c) . (takeWhileInBoard b)

riderMvmts :: Int -> Int -> ChessCoord -> [ChessCoord]
riderMvmts ri fi origin = drop 1 $ zipWith ChessCoord ranks files
    where sr = boardRank origin
          sf = boardFile origin
          ranks = [sr, sr + ri]
          files = [sf, sf + fi]

hopperMvmts :: [ChessCoord] -> ChessCoord -> [ChessCoord]
hopperMvmts incs (ChessCoord sr sf) = (tformCoord (sr+) (sf+)) <$> incs

pieceMoves :: ChessPieceClass
           -> ChessBoard
           -> ChessColor
           -> ChessCoord
           -> [ChessCoord]
pieceMoves King _ _ coord = hopperMvmts (toCoord <$> kingOffsets) coord
    where kingOffsets = filter (/= (0,0)) $ range ((-1, -1), (1,1))
          toCoord (a,b) = ChessCoord a b

pieceMoves Knight b color coord = rankHigh ++ fileHigh
    where baseMvmts = [(-1,-1), (-1,1), (1,-1), (1,1)]
          toCoord (a,b) = ChessCoord a b
          rankHigh = hopperMvmts ((toCoord . (bimap (2*) id)) <$> baseMvmts) coord
          fileHigh = hopperMvmts ((toCoord . (bimap id (2*))) <$> baseMvmts) coord

pieceMoves Bishop b color origin = ne ++ nw ++ se ++ sw
    where ne = takeToFirstCapture b color $ riderMvmts 1 1 origin
          nw = takeToFirstCapture b color $ riderMvmts 1 (-1) origin
          se = takeToFirstCapture b color $ riderMvmts (-1) 1 origin
          sw = takeToFirstCapture b color $ riderMvmts (-1) (-1) origin

pieceMoves Rook b color origin = up ++ down ++ left ++ right
    where up = takeToFirstCapture b color $ riderMvmts 1 0 origin
          down = takeToFirstCapture b color $ riderMvmts (-1) 0 origin
          left = takeToFirstCapture b color $ riderMvmts 0 (-1) origin
          right = takeToFirstCapture b color $ riderMvmts 0 1 origin

pieceMoves Queen b color origin = (pieceMoves Rook b color origin) ++ (pieceMoves Bishop b color origin)

-- TODO: Actually fill this in
pieceMoves Pawn b color origin = [origin]