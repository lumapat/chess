{-# LANGUAGE OverloadedStrings #-}

module CLI (
    runCLI,
) where

import Control.Applicative ((*>))
import Control.Monad (sequence_)
import Data.Either (isRight)
import System.IO

data CLIState = ContinueState | EndState
data Turn = WhiteTurn | BlackTurn
type CLIStep = (Turn, CLIState)

turnPrompt :: Turn -> String
turnPrompt WhiteTurn = "(White to play) "
turnPrompt BlackTurn = "(Black to play) "

nextTurn WhiteTurn = BlackTurn
nextTurn BlackTurn = WhiteTurn

prompt :: Turn -> IO CLIStep
prompt turn = do
    cmd <- putStr (turnPrompt turn) *> hFlush stdout *> getLine
    process cmd
    where
        process "quit" = putStrLn "Quitting" *> return (turn, EndState)
        process input  = putStrLn ("You typed '" ++ input ++ "'") *> return (nextTurn turn, ContinueState)

-- TODO: Need the following
--   Chess engine injection (to validate commands)
runCLI :: IO ()
runCLI = untilQuit prompt WhiteTurn

untilQuit :: (Turn -> IO CLIStep)
          -> Turn
          -> IO ()
untilQuit process turn = (process turn) >>= untilQuit'
    where
        untilQuit' :: CLIStep -> IO ()
        untilQuit' (_, EndState) = return ()
        untilQuit' (nextTurn, _) = untilQuit process nextTurn