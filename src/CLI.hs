{-# LANGUAGE OverloadedStrings #-}

module CLI (
    runCLI,
) where

import Control.Applicative ((*>))
import Control.Monad (sequence_)
import Data.Either (isRight)
import System.IO


data Turn = WhiteTurn | BlackTurn

turnPrompt :: Turn -> String
turnPrompt WhiteTurn = "(White to play) "
turnPrompt BlackTurn = "(Black to play) "

prompt :: Turn -> IO (Either String ())
prompt turn = do
    cmd <- putStr (turnPrompt turn) *> hFlush stdout *> getLine
    process cmd
    where
        process "quit" = return $ Left "Quitting..."
        process input  = putStrLn ("You typed '" ++ input ++ "'") *> return (Right ())

-- TODO: Need the following
--   Ability to quit (user quit, retries, game win)
--   Chess engine injection (to validate commands)
runCLI :: IO ()
runCLI = sequence_ $ (prompt . toTurn) <$> [0..]
    where
        toTurn n | n `mod` 2 == 0 = WhiteTurn
                 | otherwise  = BlackTurn