module Commands where

import Control.Monad.State
  ( MonadIO (liftIO),
    StateT,
  )
import CommandMove (move)
import DataTypes (Direction, GameState)
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

cmdQuit :: StateT GameState IO Bool
cmdQuit = do
  return False
