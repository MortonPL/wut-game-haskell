module CommandMove where

import Control.Monad.State
import DataTypes (Direction (East, North, South, West), GameState, Level (lv_size, lv_tiles), Player (pl_position), Tile (NoTile, DeepWater))
import Level
import Pirates
import Printer
import Random

-- [HELPER] - Turns the direction into a position delta vector
dir2Tuple :: Direction -> (Int, Int)
dir2Tuple North = (0, -1)
dir2Tuple South = (0, 1)
dir2Tuple East = (1, 0)
dir2Tuple West = (-1, 0)

-- [HELPER] - Adds 2 vectors together
add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

-- [HELPER] - Turns the direction into a number representation
dir2Num :: Direction -> Int
dir2Num North = 0
dir2Num East = 1
dir2Num South = 2
dir2Num West = 3

-- [HELPER] - Turns a number direction representation into a direction
num2Dir :: Int -> Direction
num2Dir 0 = North
num2Dir 1 = East
num2Dir 2 = South
num2Dir 3 = West
num2Dir v = num2Dir $ v `mod` 4

-- [INTERFACE] - Performs a player move
move :: Direction -> StateT GameState IO Bool
move dir = do
  state <- get
  case dir of
    North -> if snd (pl_position state) == 0 then failMove dir else succMove dir
    South -> if snd (pl_position state) == snd (lv_size level) - 1 then failMove dir else succMove dir
    East -> if fst (pl_position state) == fst (lv_size level) - 1 then failMove dir else succMove dir
    West -> if fst (pl_position state) == 0 then failMove dir else succMove dir
  return True

-- [HELPER] - Failed player move (map boundary)
failMove :: Direction -> StateT GameState IO ()
failMove dir = do
  liftIO $ println $ "  - No money to be made up " ++ show dir

-- [HELPER] - Successful player move
succMove :: Direction -> StateT GameState IO ()
succMove dir = do
  movePlayer $ dir2Tuple dir
  liftIO $ println $ "  - Moved " ++ show dir
  tryStorm dir
  tryPirates

-- [HELPER] - Changes the player position
movePlayer :: (Int, Int) -> StateT GameState IO ()
movePlayer vec = do
  state <- get
  put state {pl_position = pl_position state `add` vec}

-- [HELPER] - Returns the tile under specified coordinates
getTile :: (Int, Int) -> StateT GameState IO Tile
getTile (x, y) = do
  let (mx, my) = lv_size level
  if x < 0 || mx <= x || y < 0 || my <= y
    then return NoTile
    else return ((lv_tiles level !! x) !! y)

-- [HELPER] - Checks for a storm event
tryStorm :: Direction -> StateT GameState IO ()
tryStorm dir = do
  state <- get
  tile <- getTile $ pl_position state
  rand <- randInt (0, 2)
  when ((tile == DeepWater) && (rand == 0)) $ doStorm dir

-- [HELPER] - Performs the storm event
doStorm :: Direction -> StateT GameState IO ()
doStorm dir = do
  randRot <- randInt (-1, 1)
  movePlayer $ dir2Tuple $ num2Dir (randRot + dir2Num dir)
  liftIO $ println "  - A storm hit you!"
