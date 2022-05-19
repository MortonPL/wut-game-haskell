module Main where

import Commands (cmdHelp, cmdInvalid, cmdMove, cmdQuit, showInventory)
import Control.Monad (when)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT (..), evalStateT)
import DataTypes (Direction (East, North, South, West), GameState, Player (pl_position))
import GameState (initGameState)
import Printer (menu, println, splash)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  Printer.splash
  Printer.menu
  liftIO (evalStateT gameLoop initGameState)

gameLoop :: StateT GameState IO ()
gameLoop = do
  continue <- handleCommand
  state <- get
  liftIO $ println $ show $ pl_position state
  when continue gameLoop

readCommand :: IO String
readCommand = do
  putStr "> "
  hFlush stdout
  getLine

handleCommand :: StateT GameState IO Bool
handleCommand = do
  raw_input <- liftIO readCommand
  let input = words raw_input
  let cmd = head input
  let args = tail input
  case cmd of
    c | c `elem` ["n", "north"] -> mvn c args
    c | c `elem` ["e", "east"] -> mve c args
    c | c `elem` ["w", "west"] -> mvw c args
    c | c `elem` ["s", "south"] -> mvs c args
    c | c `elem` ["h", "help"] -> hlp c args
    c | c `elem` ["i", "inventory"] -> inv c args
    c | c `elem` ["q", "quit"] -> qit c args
    _ -> cmdInvalid
  where
    mve = noArgs $ cmdMove East
    mvs = noArgs $ cmdMove South
    mvw = noArgs $ cmdMove West
    mvn = noArgs $ cmdMove North
    inv = noArgs showInventory
    hlp = noArgs cmdHelp
    qit = noArgs cmdQuit

noArgs :: StateT GameState IO Bool -> String -> [String] -> StateT GameState IO Bool
noArgs func cmd args = do
  if null args
    then func
    else do
      liftIO $ println $ "Command `" ++ cmd ++ "` does not take any arguments!"
      return True

xArgs :: ([String] -> StateT GameState IO Bool) -> String -> [String] -> Int -> StateT GameState IO Bool
xArgs func cmd args expected = do
  if length args == expected
    then func args
    else do
      liftIO $ println $ "Command `" ++ cmd ++ "` takes exactly " ++ show expected ++ " arguments!"
      return True