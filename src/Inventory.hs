module Inventory where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe

newtype Inventory = Inventory (Map String Int)

instance Show Inventory where
  show (Inventory invMap) =
    if not $ null items then
      unlines $ "Your pockets are full with:" : map showItem items
    else
      "Your pockets are full of hopes and dreams."
    where
      items = filter countFilter (Map.toList invMap)

countFilter :: (String, Int) -> Bool
countFilter x =
  0 < snd x

showItem :: (String, Int) -> String
showItem (item, count) =
  " - " ++ show count ++ " of " ++ show item

count :: Inventory -> String -> Int
count (Inventory invMap) item =
  Data.Maybe.fromMaybe 0 (Map.lookup item invMap)

update :: Inventory -> String -> Int -> Inventory
update (Inventory invMap) item count =
  Inventory $ Map.adjust (+ count) item invMap

evaluate :: Inventory -> Map String Int -> Int
evaluate (Inventory invMap) valueMap =
  0
