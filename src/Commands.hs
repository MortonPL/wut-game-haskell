module Commands where

import CommandMove (move)
import Control.Monad.State
  ( MonadIO (liftIO),
    StateT, MonadState (get)
  )
import DataTypes (Direction, GameState, Player (pl_inventory))
import Level (level)
import Printer


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

showInventory :: StateT GameState IO Bool
showInventory = do
  state <- get
  liftIO $ println $ show $ pl_inventory state
  return True

cmdQuit :: StateT GameState IO Bool
cmdQuit = do
  return False
