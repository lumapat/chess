{-# LANGUAGE OverloadedStrings #-}

module CLI
  ( CLIProcessor (..),
    nextTurn,
    runCLI,
  )
where

import Chess.Rules (Turn (..))
import Control.Monad (sequence_)
import Data.Either (isRight)
import Data.Functor (($>))
import System.IO (hFlush, stdout)

data CLIState = ContinueState | EndState

type CLIStep a = (a, Turn, CLIState)

class CLIProcessor a where
  showBoard :: a -> String
  playMove :: a -> (Turn, String) -> Either String (a, Turn)

turnPrompt :: Turn -> String
turnPrompt WhiteTurn = "(White to play) "
turnPrompt BlackTurn = "(Black to play) "

nextTurn WhiteTurn = BlackTurn
nextTurn BlackTurn = WhiteTurn

prompt :: CLIProcessor a => CLIStep a -> IO (CLIStep a)
prompt (chessProg, turn, _) = do
  cmd <- putStr (turnPrompt turn) *> hFlush stdout *> getLine
  process cmd
  where
    process "q" = process "quit"
    process "quit" = putStrLn "Quitting..." $> (chessProg, turn, EndState)
    process "h" = process "help"
    process "help" = putStrLn "TODO" $> (chessProg, turn, ContinueState)
    process "show" = putStrLn (showBoard chessProg) $> (chessProg, turn, ContinueState)
    process input = processPlay (chessProg, turn, ContinueState) input

processPlay :: CLIProcessor a => CLIStep a -> String -> IO (CLIStep a)
processPlay step@(_, _, EndState) _ = return step
processPlay (chessProg, turn, state) move = process' $ playMove chessProg (turn, move)
  where
    process' (Left error) = putStrLn ("Got error: " ++ error) $> (chessProg, turn, state)
    process' (Right (newProg, nextTurn)) = putStrLn ("Played: " ++ move) $> (newProg, nextTurn, state)

runCLI :: CLIProcessor a => a -> IO ()
runCLI chessProg = untilQuit prompt (chessProg, WhiteTurn, ContinueState)

untilQuit ::
  CLIProcessor a =>
  (CLIStep a -> IO (CLIStep a)) ->
  CLIStep a ->
  IO ()
untilQuit _ (_, _, EndState) = return ()
untilQuit process step = process step >>= untilQuit process
