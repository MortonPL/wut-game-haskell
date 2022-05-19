module DataTypes where

import Data.Map (Map)
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
  deriving (Eq)

data Level = Level
  { lv_size :: (Int, Int),
    lv_tiles :: [[Tile]]
  }

newtype Inventory = Inventory (Map String Int)

-- [SHOW]
instance Show Inventory where
  show (Inventory invMap) =
    if not $ null items
      then unlines $ "Your pockets are full with:" : map showItem items
      else "Your pockets are full of hopes and dreams."
    where
      items = filter countFilter (Map.toList invMap)

-- [HELPER] - Filters only items with positive counts
countFilter :: (String, Int) -> Bool
countFilter x =
  snd x > 0

-- [HELPER] - Generates a string for a single item type
showItem :: (String, Int) -> String
showItem (item, count) =
  " - " ++ show count ++ " of " ++ show item

data Player = Player
  { pl_name :: String,
    pl_position :: (Int, Int),
    pl_inventory :: Inventory
  }

type GameState = Player
