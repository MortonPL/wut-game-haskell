module CommandTrad (sell, buy, toMerchant) where

import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT)
import Data.Function ((&))
import Data.Map ((!))
import DataTypes (GameState, Player (pl_inventory, pl_position), Inventory, Level (lv_tiles), Merchant (mc_selling, mc_buying), Tile (Island))
import Inventory (count, update)
import Printer (println)
import Level (itemValues, level)
import qualified Data.Map as Map

countCoins :: Inventory -> Int
countCoins invMap = count invMap "coin"

updateCoins :: Inventory -> Int -> Inventory
updateCoins invMap = update invMap "coin"

toMerchant :: (Int, Int) -> Maybe Merchant
toMerchant position =
  case currentField of
    Island _ m -> Just m
    _ -> Nothing
  where currentField = lv_tiles level !! fst position !! snd position

buys :: Merchant -> String -> Bool
buys merchant itemName = case Map.lookup itemName (mc_buying merchant) of
  Just _ -> True
  _ -> False

sells :: Merchant -> String -> Bool
sells merchant itemName = case Map.lookup itemName (mc_selling merchant) of
  Just _ -> True
  _ -> False

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
  let maybeMerchant = toMerchant $ pl_position state
  case maybeMerchant of
    Nothing -> do
      liftIO $ println $ textCannotSell itemName
      return True
    Just m | not (buys m itemName) -> do
      liftIO $ println $ textCannotSell itemName
      return True
    Just m -> sellToMerchant m itemName amount

sellToMerchant :: Merchant -> String -> Int -> StateT GameState IO Bool
sellToMerchant merchant itemName amount = do
  state <- get
  let currentAmount = count (pl_inventory state) itemName
  case amount of
    i | i > currentAmount -> do
      liftIO $ println $ textNotEnoughItems itemName
      return True
    _ -> do
      let basePrice = fromIntegral (itemValues ! itemName)
      let priceModifier = mc_buying merchant ! itemName
      let totalPrice = amount * round (basePrice * priceModifier)
      updateInvOnSellSucc itemName amount totalPrice

updateInvOnSellSucc :: String -> Int -> Int -> StateT GameState IO Bool
updateInvOnSellSucc itemName amount totalPrice = do
  state <- get
  let oldInv = pl_inventory state
  let partiallyUpdatedInv = update oldInv itemName (-amount)
  let updatedInv = updateCoins partiallyUpdatedInv totalPrice
  put state {pl_inventory = updatedInv}
  liftIO $ println $ textSoldSuccessfully itemName amount totalPrice
  return True

buy :: String -> Int -> StateT GameState IO Bool
buy itemName amount = do
  state <- get
  let maybeMerchant = toMerchant $ pl_position state
  case maybeMerchant of
    Nothing -> do
      liftIO $ println $ textCannotBuy itemName
      return True
    Just m | not (sells m itemName) -> do
      liftIO $ println $ textCannotBuy itemName
      return True
    Just m -> buyFromMerchant m itemName amount

buyFromMerchant :: Merchant -> String -> Int -> StateT GameState IO Bool
buyFromMerchant merchant itemName amount = do
  state <- get
  let basePrice = fromIntegral (itemValues ! itemName)
  let priceModifier = mc_selling merchant ! itemName
  let totalPrice = amount * round (priceModifier * basePrice)
  let currentMoney = countCoins (pl_inventory state)
  case totalPrice of
    i | i > currentMoney -> do
      liftIO $ println textNotEnoughMoney
      return True
    _ -> updateInvOnBuySucc itemName amount totalPrice

updateInvOnBuySucc :: String -> Int -> Int -> StateT GameState IO Bool
updateInvOnBuySucc itemName amount totalPrice = do
  state <- get
  let oldInv = pl_inventory state
  let partiallyUpdatedInv = updateCoins oldInv (-totalPrice)
  let updatedInv = update partiallyUpdatedInv itemName amount
  put state {pl_inventory = updatedInv}
  liftIO $ println $ textBoughtSuccessfully itemName amount totalPrice
  return True
