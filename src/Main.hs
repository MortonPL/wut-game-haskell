module Main where

import Commands (cmdAppraise, cmdAsk, cmdBuy, cmdHelp, cmdInvalid, cmdInventory, cmdLook, cmdMove, cmdQuit, cmdSell, cmdSave, cmdLoad,)
import Control.Monad (when)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT (..), evalStateT)

import DataTypes (Direction (East, North, South, West), GameState, Player (pl_position))
import GameState (initGameState)
import Printer (menu, println, splash)
import System.IO (hFlush, stdout)

-- [INTERFACE] - Entry point.
main :: IO ()
main = do
  Printer.splash
  liftIO (evalStateT gameLoop initGameState)

-- [HELPER] - Performs the game loop until terminated by `quit` command.
gameLoop :: StateT GameState IO ()
gameLoop = do
  continue <- handleCommand
  when continue gameLoop

-- [HELPER] - Awaits player input.
readCommand :: IO String
readCommand = do
  putStr "> "
  hFlush stdout
  getLine

-- [HELPER] - Runs commands based on player input.
handleCommand :: StateT GameState IO Bool
handleCommand = do
  raw_input <- liftIO readCommand
  let input = words raw_input
  let cmd = head input
  let args = tail input
  case cmd of
    c | c `elem` ["n", "north"]     -> mvn args c
    c | c `elem` ["e", "east"]      -> mve args c
    c | c `elem` ["w", "west"]      -> mvw args c
    c | c `elem` ["s", "south"]     -> mvs args c
    c | c `elem` ["b", "buy"]       -> buy args c
    c | c `elem` ["z", "sell"]      -> sel args c
    c | c `elem` ["k", "ask"]       -> ask args c
    c | c `elem` ["h", "help"]      -> hlp args c
    c | c `elem` ["i", "inventory"] -> inv args c
    c | c `elem` ["q", "quit"]      -> qit args c
    c | c `elem` ["a", "appraise"]  -> ins args c
    c | c `elem` ["l", "look"]      -> lok args c
    c | c `elem` ["v", "save"]      -> sav args c
    c | c `elem` ["d", "load"]      -> lod args c
    _ -> cmdInvalid
  where
    mve = noArgs $ cmdMove East
    mvs = noArgs $ cmdMove South
    mvw = noArgs $ cmdMove West
    mvn = noArgs $ cmdMove North
    buy = xArgs 2 cmdBuy
    sel = xArgs 2 cmdSell
    inv = noArgs cmdInventory
    hlp = noArgs cmdHelp
    qit = noArgs cmdQuit
    ins = xArgs 1 cmdAppraise
    lok = noArgs cmdLook
    ask = xArgs 2 cmdAsk
    sav = xArgs 1 cmdSave
    lod = xArgs 1 cmdLoad

-- [HELPER] - Validates argument count for 0-argument commands.
noArgs :: StateT GameState IO Bool -> [String] -> String -> StateT GameState IO Bool
noArgs func args cmd = do
  if null args
    then func
    else do
      liftIO $ println $ "Command `" ++ cmd ++ "` does not take any arguments!"
      return True

-- [HELPER] - Validates argument count for N-argument commands.
xArgs :: Int -> ([String] -> StateT GameState IO Bool) -> [String] -> String -> StateT GameState IO Bool
xArgs expected func args cmd = do
  if length args == expected
    then func args
    else do
      liftIO $ println $ "Command `" ++ cmd ++ "` takes exactly " ++ show expected ++ " arguments!"
      return True
