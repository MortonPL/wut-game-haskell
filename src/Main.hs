module Main where

import Control.Monad (when)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT (..), evalStateT)
import System.IO (hFlush, stdout)

import Commands (cmdHelp, cmdInvalid, cmdMove, cmdQuit, showInventory)
import DataTypes (Direction (East, North, South, West), GameState, Player (pl_position))
import GameState (initGameState)
import Printer (menu, println, splash)

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
  cmd <- liftIO readCommand
  case cmd of
    c | c `elem` ["e", "east"] -> cmdMove East
    c | c `elem` ["h", "help"] -> cmdHelp
    c | c `elem` ["i", "inventory"] -> showInventory
    c | c `elem` ["n", "north"] -> cmdMove North
    c | c `elem` ["q", "quit"] -> cmdQuit
    c | c `elem` ["s", "south"] -> cmdMove South
    c | c `elem` ["w", "west"] -> cmdMove West
    _ -> cmdInvalid
