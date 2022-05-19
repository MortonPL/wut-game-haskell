module DataTypes where

import Data.Map (Map)
import qualified Data.Map as Map
import Inventory (Inventory)

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

data Player = Player
  { pl_name :: String,
    pl_position :: (Int, Int),
    pl_inventory :: Inventory
  }

type GameState = Player
