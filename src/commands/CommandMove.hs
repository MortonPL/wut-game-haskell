module CommandMove where

import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get, put),
    StateT,
    when,
  )

import DataTypes (Direction (East, North, South, West), GameState, Level (lv_size, lv_tiles), Tile (DeepWater), pl_position)
import Level (level)
import Printer (println)
import Random (randInt)

dir2Tuple :: Direction -> (Int, Int)
dir2Tuple North = (0, -1)
dir2Tuple South = (0, 1)
dir2Tuple East = (1, 0)
dir2Tuple West = (-1, 0)

dir2Num :: Direction -> Int
dir2Num North = 0
dir2Num East = 1
dir2Num South = 2
dir2Num West = 3

num2Dir :: Int -> Direction
num2Dir 0 = North
num2Dir 1 = East
num2Dir 2 = South
num2Dir 3 = West
num2Dir v = num2Dir $ v `mod` 4

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

move :: Direction -> StateT GameState IO Bool
move dir = do
  state <- get
  case dir of
    North -> if snd (pl_position state) == 0 then failMove dir else succMove dir
    South -> if snd (pl_position state) == snd (lv_size level) - 1 then failMove dir else succMove dir
    East -> if fst (pl_position state) == fst (lv_size level) - 1 then failMove dir else succMove dir
    West -> if fst (pl_position state) == 0 then failMove dir else succMove dir
  tryStorm dir
  return True

failMove :: Direction -> StateT GameState IO ()
failMove dir = do
  liftIO $ println $ "  - No money to be made up " ++ show dir

succMove :: Direction -> StateT GameState IO ()
succMove dir = do
  movePlayer $ dir2Tuple dir
  liftIO $ println $ "  - Moved " ++ show dir

movePlayer :: (Int, Int) -> StateT GameState IO ()
movePlayer vec = do
  state <- get
  put state {pl_position = pl_position state `add` vec}

tryStorm :: Direction -> StateT GameState IO ()
tryStorm dir = do
  tile <- getTile
  rand <- randInt (0, 2)
  when ((tile == DeepWater) && (rand == 0)) $ doStorm dir

doStorm :: Direction -> StateT GameState IO ()
doStorm dir = do
  randRot <- randInt (-1, 1)
  movePlayer $ dir2Tuple $ num2Dir (randRot + dir2Num dir)
  liftIO $ println "  - A storm hit you!"

getTile :: StateT GameState IO Tile
getTile = do
  state <- get
  return ((lv_tiles level !! fst (pl_position state)) !! snd (pl_position state))
