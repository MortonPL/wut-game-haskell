module CommandLook where

import CommandMove (add, dir2Tuple, getTile)
import Control.Monad.State
import DataTypes
import Printer (printLines, println)
import Random (randInt)

-- [INTERFACE] - Prints description of the surrounding 4 tiles.
look :: StateT GameState IO Bool
look = do
  current <- describeCurrentTile
  liftIO $ println "Your first mate reports:"
  liftIO $ printLines $ map ("  - " ++) current
  n <- lookAt North
  e <- lookAt East
  w <- lookAt West
  s <- lookAt South
  liftIO $ println "Your foretopman reports:"
  liftIO $ printLines $ map ("  - " ++) [n, w, s, e]
  return True

-- [HELPER] - Prints the description of one direction.
lookAt :: Direction -> StateT GameState IO String
lookAt dir = do
  state <- get
  let pos = pl_position state
  let npos = pos `add` dir2Tuple dir
  tile <- getTile npos
  t <- describeTile tile
  return $ t ++ " up " ++ show dir ++ "."

-- [HELPER] - Returns the description of the tile.
describeTile :: Tile -> StateT GameState IO String
describeTile tile =
  case tile of
    Island _ _ -> describeIsland
    ShallowWater -> describeShallow
    DeepWater -> describeDeep
    NoTile -> describeNoTile

-- [HELPER] - Island variants.
describeIsland :: StateT GameState IO String
describeIsland = do
  r <- randInt (0, 2)
  case r of
    0 -> return "You spot a piece of land"
    1 -> return "You notice a landrats nest"
    _ -> return "Your barrelman sees land"

-- [HELPER] - Shallow water variants.
describeShallow :: StateT GameState IO String
describeShallow = do
  r <- randInt (0, 2)
  case r of
    0 -> return "You see a calm sea"
    1 -> return "The weather is beautiful"
    _ -> return "Just the borring 'ol sea"

-- [HELPER] - Deep water variants.
describeDeep :: StateT GameState IO String
describeDeep = do
  r <- randInt (0, 2)
  case r of
    0 -> return "The sky is about to fall"
    1 -> return "You see the ocean raging"
    _ -> return "You are gonna have a bad time"

-- [HELPER] - Border variants.
describeNoTile :: StateT GameState IO String
describeNoTile = do
  r <- randInt (0, 2)
  case r of
    0 -> return "You smell no money"
    1 -> return "No adventure awaits you"
    _ -> return "Nothing of interest"

-- [HELPER] - Returns the description of the current tile.
describeCurrentTile :: StateT GameState IO [String]
describeCurrentTile = do
  state <- get
  tile <- getTile $ pl_position state
  case tile of
    Island name merchant -> return
      ["You are on the " ++ name ++ ".", "Rumours are you can trade with " ++ mc_name merchant ++ " here. You gotta ask them."]
    ShallowWater -> return ["You are on a calm sea."]
    DeepWater -> return ["The sea is unsteady.", "Your ship could be blown away at any moment..."]
    NoTile -> return []
