module CommandLook where

import CommandMove (add, dir2Tuple, getTile)
import Control.Monad.State
import DataTypes
import Printer (printLines, println)
import Random (randInt)

look :: StateT GameState IO Bool
look = do
  n <- lookAt North
  e <- lookAt East
  w <- lookAt West
  s <- lookAt South
  liftIO $ printLines [n, w, s, e]
  return True

lookAt :: Direction -> StateT GameState IO String
lookAt dir = do
  state <- get
  let pos = pl_position state
  let npos = pos `add` dir2Tuple dir
  tile <- getTile npos
  t <- describeTile tile
  return $ t ++ " up " ++ show dir ++ "."

describeTile :: Tile -> StateT GameState IO String
describeTile tile =
  case tile of
    Island _ _ -> describeIsland
    ShallowWater -> describeShallow
    DeepWater -> describeDeep
    NoTile -> describeNoTile

describeIsland :: StateT GameState IO String
describeIsland = do
  r <- randInt (0, 2)
  case r of
    0 -> return "You spot a piece of land"
    1 -> return "You notice a landrats nest"
    _ -> return "Your barrelman sees land"

describeShallow :: StateT GameState IO String
describeShallow = do
  r <- randInt (0, 2)
  case r of
    0 -> return "You see a calm sea"
    1 -> return "The weather is beautiful"
    _ -> return "Just the borring 'ol sea"

describeDeep :: StateT GameState IO String
describeDeep = do
  r <- randInt (0, 2)
  case r of
    0 -> return "The sky is about to fall"
    1 -> return "You see the ocean raging"
    _ -> return "You are gonna have a bad time"

describeNoTile :: StateT GameState IO String
describeNoTile = do
  r <- randInt (0, 2)
  case r of
    0 -> return "You smell no money"
    1 -> return "No adventure awaits you"
    _ -> return "Nothing of interest"
