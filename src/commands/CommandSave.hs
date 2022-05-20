module CommandSave where

import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (..))
import DataTypes (GameState, Player (Player, pl_name, pl_inventory, pl_position), InventoryS (InventoryS), Inventory (Inventory))

save :: String -> StateT GameState IO Bool
save fileName = do
  state <- get
  let gamedata = show (pl_name state) ++ "\n"
              ++ show (pl_position state) ++ "\n"
              ++ show (InventoryS (pl_inventory state))
  liftIO $ writeFile ("saves\\" ++ fileName) gamedata
  return True

load :: String -> StateT GameState IO Bool
load fileName = do
  raw <- liftIO $ readFile ("saves\\" ++ fileName)
  state <- get
  let parsed = words raw
  let name = head parsed :: String
  let pos = read $ parsed!!1 :: (Int, Int)
  let inv = read $ unwords (tail $ tail parsed) :: [(String, Int)]
  put state {pl_name = name, pl_position = pos, pl_inventory = Inventory $ Map.fromList inv}
  return True
