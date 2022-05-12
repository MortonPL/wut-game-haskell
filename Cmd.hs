module Cmd
  ( handleCommand,
  )
where

import Control.Monad.State (State, StateT)
import GHC.IO.Handle.Internals (flushBuffer)
import System.IO (hFlush, stdout)

data Direction
  = North
  | South
  | West
  | East
  deriving (Eq, Show)

data Merchant
  = String

data Tile
  = ShallowWater
  | DeepWater
  | Island String Merchant

data Level = Level
  { size :: (Int, Int),
    tiles :: [[Tile]]
  }

data Player = Player
  { name :: String,
    position :: (Int, Int)
  }

data GameState = GameState
  { level :: Level,
    player :: Player
  }

gameState =
  GameState
    { level =
        Level
          { size = (6, 6),
            tiles =
              [ []
              ]
          },
      player =
        Player
          { name = "hahe",
            position = (0, 0)
          }
    }

type GameStateIO = StateT gameState IO (*)

readCommand :: IO String
readCommand = do
  putStr "> "
  hFlush stdout
  getLine

handleCommand :: IO ()
handleCommand = do
  cmd <- readCommand
  case cmd of
    "n" -> move North
    "north" -> move North
    "s" -> move South
    "south" -> move South
    "e" -> move East
    "east" -> move East
    "w" -> move West
    "west" -> move West
    _ -> print "Invalid command!"

move :: Direction -> IO ()
move dir = do
  print dir
