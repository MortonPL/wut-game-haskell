module Pirates where

import Control.Monad.State
import DataTypes
import GHC.Float
import Inventory
import Level
import Printer
import Random

-- [INTERFACE] - Attempts the pirate event.
tryPirates :: StateT GameState IO ()
tryPirates = do
  state <- get
  let attr = evaluate $ pl_inventory state
  when (attr > pirAttMin) (rollPirates attr)

-- [HELPER] - Chance for pirate event based on player inventory value.
rollPirates :: Int -> StateT GameState IO ()
rollPirates attr = do
  let t = int2Float (attr - pirAttMin) / int2Float (pirAttMax - pirAttMin)
  let t' = min t 1.0
  let p = t' * pirMaxProb
  r <- randFloat
  when (r < p) piratesAttack

-- [HELPER] - Performs the pirate attack event.
piratesAttack :: StateT GameState IO ()
piratesAttack = do
  state <- get
  let inv = pl_inventory state
  let mercs = count inv "mercenary"
  liftIO $ println "You've been attacked by pirates."
  if mercs > 0
    then defendFromPirates
    else loseToPirates

-- [HELPER] - Pirate attack gets defended.
defendFromPirates :: StateT GameState IO ()
defendFromPirates = do
  state <- get
  let inv = pl_inventory state
  let ninv = update inv "mercenary" $ -1
  put state {pl_inventory = ninv}
  liftIO $
    printLines
      [ "Your brave mercenaries have protected you.",
        "-1 mercenary"
      ]
  giveMapPiece

-- [HELPER] - Pirate attack goes through.
loseToPirates :: StateT GameState IO ()
loseToPirates = do
  state <- get
  let inv = pl_inventory state
  let ninv = halveItems inv
  put state {pl_inventory = ninv}
  liftIO $
    printLines
      [ "They stole half of your possessions.",
        "Try hiring some guards next time!"
      ]

-- [HELPER] - Checks whether the player should get a map piece.
tryGiveMapPiece :: StateT GameState IO ()
tryGiveMapPiece = do
  state <- get
  let inv = pl_inventory state
  when (count inv "map_piece_2" == 0) giveMapPiece

-- [HELPER] - Gives player the map piece.
giveMapPiece :: StateT GameState IO ()
giveMapPiece = do
  state <- get
  let inv = pl_inventory state
  let ninv = update inv "map_piece_2" 1
  put state {pl_inventory = ninv}
  liftIO $ println "In the wreckage you found a map piece!"
