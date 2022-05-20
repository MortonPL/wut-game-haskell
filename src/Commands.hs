module Commands where

import CommandAppr (appraise)
import CommandLook (look)
import CommandMove (move)
import CommandTrad (buy, sell)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT)
import DataTypes (Direction, GameState, Player (pl_inventory))
import Printer

-- [INTERFACE] - The `help` command.
cmdHelp :: StateT GameState IO Bool
cmdHelp = do
  liftIO Printer.help
  return True

-- [INTERFACE] - Invalid command.
cmdInvalid :: StateT GameState IO Bool
cmdInvalid = do
  liftIO $ print "Invalid command!"
  return True

-- [INTERFACE] - Collection of `north`, `south`, `west` and `east` commands.
cmdMove :: Direction -> StateT GameState IO Bool
cmdMove = move

-- [INTERFACE] - Sell to a merchant command.
cmdSell :: [String] -> StateT GameState IO Bool
cmdSell args = sell itemName amount
  where
    itemName = head args
    amount = read (args !! 1)

-- [INTERFACE] - Buy from a merchant command.
cmdBuy :: [String] -> StateT GameState IO Bool
cmdBuy args = buy itemName amount
  where
    itemName = head args
    amount = read (args !! 1)

-- [INTERFACE] - Appraise an item command.
cmdAppraise :: [String] -> StateT GameState IO Bool
cmdAppraise args = appraise $ head args

-- [INTERFACE] - Look around command.
cmdLook :: StateT GameState IO Bool
cmdLook = look

-- [INTERFACE] - Look at inventory command.
cmdInventory :: StateT GameState IO Bool
cmdInventory = do
  state <- get
  liftIO $ println $ show $ pl_inventory state
  return True

-- [INTERFACE] - Quit the game command.
cmdQuit :: StateT GameState IO Bool
cmdQuit = do
  return False
