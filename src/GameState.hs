module GameState where

import Data.Map (Map)
import qualified Data.Map as Map
import DataTypes (Player (Player, pl_inventory, pl_name, pl_position))
import Inventory (Inventory (Inventory))

initGameState :: Player
initGameState =
  Player
    { pl_name = "joe",
      pl_position = (0, 0),
      pl_inventory = Inventory $ Map.fromList [("balls", 2), ("in your:", 0)]
    }
