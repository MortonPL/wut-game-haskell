module Commands where

import Control.Monad.State (MonadIO (liftIO), StateT, MonadState (get, put))
import DataTypes (Direction, GameState, Player (pl_inventory))
import Printer
import CommandMove (move)
import CommandTrad (sell, buy)


cmdHelp :: StateT GameState IO Bool
cmdHelp = do
  liftIO Printer.help
  return True

cmdInvalid :: StateT GameState IO Bool
cmdInvalid = do
  liftIO $ print "Invalid command!"
  return True

cmdMove :: Direction -> StateT GameState IO Bool
cmdMove = move

cmdSell :: String -> Int -> StateT GameState IO Bool
cmdSell = sell

cmdBuy :: String -> Int -> StateT GameState IO Bool
cmdBuy = buy

showInventory :: StateT GameState IO Bool
showInventory = do
  state <- get
  liftIO $ println $ show $ pl_inventory state
  return True

cmdQuit :: StateT GameState IO Bool
cmdQuit = do
  return False
