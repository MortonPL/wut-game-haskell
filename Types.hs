module Types
  ( GameState (..)
  , Direction (..)
  , Level (..)
  , Player (..)
  , Tile (..)
  , Merchant (..)
  )
where

data Direction
  = North
  | South
  | West
  | East
  deriving (Eq, Show)

newtype Merchant
  = Merchant String

data Tile
  = ShallowWater
  | DeepWater
  | Island String Merchant

data Level = Level
  { size :: (Int, Int)
  , tiles :: [[Tile]]
  }

data Player = Player
  { name :: String
  , position :: (Int, Int)
  }

newtype GameState = GameState
  { player :: Player
  }
