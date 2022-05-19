module Inventory where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Maybe
import DataTypes (Inventory (Inventory))
import Level (itemPirAtt)

-- [INTERFACE] - Returns the amount of specific item
count :: Inventory -> String -> Int
count (Inventory invMap) item =
  Data.Maybe.fromMaybe 0 (Map.lookup item invMap)

-- [INTERFACE] - Adds the item count to inventory
update :: Inventory -> String -> Int -> Inventory
update (Inventory invMap) item count =
  Inventory $ Map.adjust (+ count) item invMap

-- [INTERFACE] - Calculates the pirate attraction of the inventory
evaluate :: Inventory -> Int
evaluate (Inventory invMap) =
  sum $ map evaluateItem $ Map.toList invMap

-- [HELPER] - Calculates the pirate attraction of a single item type
evaluateItem :: (String, Int) -> Int
evaluateItem (item, count) =
  itemPirAtt ! item * count

-- [HELPER] - Halves all items in the inventory
halveItems :: Inventory -> Inventory
halveItems (Inventory invMap) =
  Inventory $ Map.map (div 2) invMap
