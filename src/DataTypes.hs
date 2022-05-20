module DataTypes where

import Data.Map (Map)
import Data.List (intersperse, intercalate)
import qualified Data.Map as Map

data Direction
  = North
  | South
  | West
  | East
  deriving (Eq, Show)

data Merchant = Merchant
  { mc_name :: String,
    mc_desc :: String,
    mc_merchants :: String,
    mc_selling :: Map String Float,
    mc_buying :: Map String Float
  }
  deriving (Eq)

data Tile
  = ShallowWater
  | DeepWater
  | Island String Merchant
  | NoTile
  deriving (Eq)

data Level = Level
  { lv_size :: (Int, Int),
    lv_tiles :: [[Tile]]
  }

newtype Inventory = Inventory (Map String Int)

-- [SHOW] - Displays the inventory.
instance Show Inventory where
  show (Inventory invMap) =
    if null items
      then "Your pockets are full of hopes and dreams."
      else unlines $ "Your pockets are full with:" : map showItem items
    where
      items = filter countFilter (Map.toList invMap)

-- [HELPER] - Filters only items with positive counts.
countFilter :: (String, Int) -> Bool
countFilter x =
  snd x > 0

-- [HELPER] - Generates a string for a single item type.
showItem :: (String, Int) -> String
showItem (item, count) =
  " - " ++ show count ++ " of " ++ show item

newtype InventoryS = InventoryS Inventory

instance Show InventoryS where
  show (InventoryS (Inventory invMap)) = "[" ++ intercalate ", " (map showItemS $ Map.toList invMap) ++ "]"

showItemS :: (String, Int) -> String
showItemS (item, count) = 
  "(" ++ show item ++ "," ++ show count ++ ")"

data Player = Player
  { pl_name :: String,
    pl_position :: (Int, Int),
    pl_inventory :: Inventory
  }

type GameState = Player
