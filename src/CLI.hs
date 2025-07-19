{-# LANGUAGE OverloadedStrings #-}

module CLI
  ( CLIDebug (..),
    CLIProcessor (..),
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

class CLIDebug a where
  debug1 :: a -> [String] -> String

class CLIProcessor a where
  showBoard :: a -> String
  playMove :: a -> (Turn, String) -> Either String (a, Turn)

turnPrompt :: Turn -> String
turnPrompt WhiteTurn = "(White to play) "
turnPrompt BlackTurn = "(Black to play) "

prompt :: (CLIProcessor a, CLIDebug a) => CLIStep a -> IO (CLIStep a)
prompt (chessProg, turn, _) = do
  cmd <- putStr (turnPrompt turn) *> hFlush stdout *> getLine
  process (words cmd)
  where
    defaultNextState = (chessProg, turn, ContinueState)
    process (cmd : args) = process' cmd args
    process [] = putStrLn "You entered nothing. Please enter a command" $> defaultNextState

    process' "debug" args = putStrLn (debug1 chessProg args) $> defaultNextState
    process' "q" args = process' "quit" args
    process' "quit" _ = putStrLn "Quitting..." $> (chessProg, turn, EndState)
    process' "h" args = process' "help" args
    process' "help" _ = putStrLn "TODO" $> defaultNextState
    process' "show" _ = putStrLn (showBoard chessProg) $> defaultNextState
    process' input _ = processPlay (chessProg, turn, ContinueState) input

processPlay :: CLIProcessor a => CLIStep a -> String -> IO (CLIStep a)
processPlay step@(_, _, EndState) _ = return step
processPlay (chessProg, turn, state) move = process' $ playMove chessProg (turn, move)
  where
    process' (Left error) = putStrLn ("Got error: " ++ error) $> (chessProg, turn, state)
    process' (Right (newProg, nextTurn)) = putStrLn ("Played: " ++ move) $> (newProg, nextTurn, state)

runCLI :: (CLIProcessor a, CLIDebug a) => a -> IO ()
runCLI chessProg = untilQuit prompt (chessProg, WhiteTurn, ContinueState)

untilQuit ::
  CLIProcessor a =>
  (CLIStep a -> IO (CLIStep a)) ->
  CLIStep a ->
  IO ()
untilQuit _ (_, _, EndState) = return ()
untilQuit process step = process step >>= untilQuit process
