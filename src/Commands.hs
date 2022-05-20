module Commands where

import CommandAppr (appraise)
import CommandLook (look)
import CommandMove (move)
import CommandTrad (buy, sell)
import CommandAskM (ask)
import CommandSave (save, load)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT)
import DataTypes (Direction, GameState, Player (pl_inventory))
import Printer
import Text.Read (readMaybe)

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
cmdSell args = case maybeAmount of
    Just amount -> sell itemName amount
    Nothing -> do
      liftIO $ println "Command `sell` expects a string (item name) and an integer (amount)!"
      return True
  where
    itemName = head args
    maybeAmount = readMaybe (args !! 1)

-- [INTERFACE] - Buy from a merchant command.
cmdBuy :: [String] -> StateT GameState IO Bool
cmdBuy args = case maybeAmount of
    Just amount -> buy itemName amount
    Nothing -> do
      liftIO $ println "Command `buy` expects a string (item name) and an integer (amount)!"
      return True
  where
    itemName = head args
    maybeAmount = readMaybe (args !! 1)

-- [INTERFACE] - Appraise an item command.
cmdAppraise :: [String] -> StateT GameState IO Bool
cmdAppraise args = appraise $ head args

-- [INTERFACE] - Look around command.
cmdLook :: StateT GameState IO Bool
cmdLook = look

-- [INTERFACE] - Ask a merchant command.
cmdAsk :: [String] -> StateT GameState IO Bool
cmdAsk (name : topic : _) = ask name topic
cmdAsk _ = do
  liftIO $ println "Command `ask` expects two strings (merchant name, topic)!"
  return True

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

cmdSave :: [String] -> StateT GameState IO Bool
cmdSave args = do
  save $ head args
  return True

cmdLoad :: [String] -> StateT GameState IO Bool
cmdLoad args = do
  load $ head args
