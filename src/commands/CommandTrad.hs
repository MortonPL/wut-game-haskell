{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CommandTrad (sell, buy) where

import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT)
import DataTypes (GameState, Player (pl_inventory), Inventory)
import Inventory (count, update)
import Printer (println)
import Data.Function ((&))

countCoins :: Inventory -> Int
countCoins invMap = count invMap "coin"

updateCoins :: Inventory -> Int -> Inventory
updateCoins invMap = update invMap "coin"

textNotEnoughItems :: String -> String
textNotEnoughItems itemName = "There is not enough '" ++ itemName ++ "' on you."

textNotEnoughMoney :: String
textNotEnoughMoney = "Come back when you're a little richer."

textSoldSuccessfully :: String -> Int -> Int -> String
textSoldSuccessfully itemName amount price = "You sold " ++ show amount ++ " of '" ++ itemName ++ "' for " ++ show price ++ " coins."

textBoughtSuccessfully :: String -> Int -> Int -> String
textBoughtSuccessfully itemName amount price = "You bought " ++ show amount ++ " of '" ++ itemName ++ "' for " ++ show price ++ " coins."

textCannotSell :: String -> String
textCannotSell itemName = "You can't sell '" ++ itemName ++ "' here."

textCannotBuy :: String -> String
textCannotBuy itemName = "You can't buy '" ++ itemName ++ "' here."

sell :: String -> Int -> StateT GameState IO Bool
sell itemName amount = do
  state <- get
  -- TODO: merchant check
  let oldInv = pl_inventory state
  let currentAmount = count oldInv itemName
  case amount of
    i | i > currentAmount -> do
      liftIO $ println $ textNotEnoughItems itemName
      return True
  let basePrice = 50  -- TODO
  let priceModifier = 1.1  -- TODO
  let totalPrice = amount * round (basePrice * priceModifier)
  let partiallyUpdatedInv = update oldInv itemName (-amount)
  let updatedInv = updateCoins partiallyUpdatedInv totalPrice
  put state {pl_inventory = updatedInv}
  liftIO $ println $ textSoldSuccessfully itemName amount totalPrice
  return True

buy :: String -> Int -> StateT GameState IO Bool
buy itemName amount = do
  state <- get
  -- TODO: merchant check
  let basePrice = 50  -- TODO
  let priceModifier = 1.1  -- TODO
  let totalPrice = amount * round (basePrice * priceModifier)
  let oldInv = pl_inventory state
  let currentMoney = countCoins oldInv
  case totalPrice of
    i | i > currentMoney -> do
      liftIO $ println textNotEnoughMoney
      return True
  let partiallyUpdatedInv = updateCoins oldInv (-totalPrice)
  let updatedInv = update partiallyUpdatedInv itemName amount
  put state {pl_inventory = updatedInv}
  liftIO $ println $ textBoughtSuccessfully itemName amount totalPrice
  return True
