module CommandAskM (ask) where

import Control.Monad.State (StateT, MonadState (get), MonadIO (liftIO))
import DataTypes (GameState, Player (pl_position), Merchant (mc_name, mc_desc, mc_selling, mc_buying))
import CommandTrad (toMerchant)
import Printer (println, printLines)
import qualified Data.Map as Map
import Level (itemValues)
import Data.Map ((!))

-- [INTERFACE] - Ask a merchant about something.
ask :: String -> String -> StateT GameState IO Bool
ask name topic = do
  state <- get
  let maybeMerchant = toMerchant (pl_position state)
  case maybeMerchant of
    Just m | mc_name m == name -> askLocalMerchant m topic
    _ -> do
      liftIO $ println $ "Unfortunately, " ++ name ++ " is far away."
      return True

-- [HELPER] - Buying/selling list entry.
describeItem :: (String, Float) -> String
describeItem (name, priceModifier) = "'" ++ name ++ "' for " ++ show (round priceModifier * basePrice) ++ " coins each"
  where basePrice = fromIntegral $ itemValues ! name

-- [HELPER] - Asking pt. 2 (after it is determined that a merchant is available at the current position).
askLocalMerchant :: Merchant -> String -> StateT GameState IO Bool
askLocalMerchant merchant "offer" = do
  liftIO $ println $ mc_desc merchant
  liftIO $ println "[Selling]"
  liftIO $ printLines $ map describeItem $ Map.toList $ mc_selling merchant
  liftIO $ println "[Buying]"
  liftIO $ printLines $ map describeItem $ Map.toList $ mc_buying merchant
  return True
askLocalMerchant merchant topic = do
  liftIO $ println "Haven't heard of it."
  return True
