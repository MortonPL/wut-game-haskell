module GameState where

import Data.Map (Map)
import qualified Data.Map as Map
import DataTypes (Inventory (Inventory), Player (Player, pl_inventory, pl_name, pl_position))

initGameState :: Player
initGameState =
  Player
    { pl_name = "joe",
      pl_position = (0, 0),
      pl_inventory = Inventory $ Map.fromList
      [ ("balls", 8)
      , ("mercenary", 1)
      , ("coin", 50)
      ]
    }
