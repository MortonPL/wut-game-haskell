module Inventory where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe

newtype Inventory = Inventory (Map String Int)

instance Show Inventory where
  show (Inventory invMap) =
    unlines $ map $ show Map.toList invMap

count :: Inventory -> String -> Int
count (Inventory invMap) item =
  Data.Maybe.fromMaybe 0 (Map.lookup item invMap)

update :: Inventory -> String -> Int -> Inventory
update (Inventory invMap) item count =
  Inventory $ Map.adjust (+ count) item invMap
