module Commands where

import CommandAppr (appraise)
import CommandLook (look)
import CommandMove (move)
import CommandTrad (buy, sell)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT)
import DataTypes (Direction, GameState, Player (pl_inventory))
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

cmdSell :: [String] -> StateT GameState IO Bool
cmdSell args = sell itemName amount
  where
    itemName = head args
    amount = read (args !! 1)

cmdBuy :: [String] -> StateT GameState IO Bool
cmdBuy args = buy itemName amount
  where
    itemName = head args
    amount = read (args !! 1)

cmdAppraise :: [String] -> StateT GameState IO Bool
cmdAppraise args = appraise $ head args

cmdLook :: StateT GameState IO Bool
cmdLook = look

showInventory :: StateT GameState IO Bool
showInventory = do
  state <- get
  liftIO $ println $ show $ pl_inventory state
  return True

cmdQuit :: StateT GameState IO Bool
cmdQuit = do
  return False
