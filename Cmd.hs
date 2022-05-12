module Cmd
  ( handleCommand
  , readCommand
  , GameState (..)
  , Level (..)
  , Player (..)
  )
where

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
  { size :: (Int, Int)
  , tiles :: [[Tile]]
  }

data Player = Player
  { name :: String
  , position :: (Int, Int)
  }

data GameState = GameState
  { level :: Level
  , player :: Player
  }

helpText :: [String]
helpText =
  [ "Available commands are:"
  , ""
  , "instructions  -- to see these instructions."
  , "quit          -- to end the game and quit."
  , ""
  ]

invalidCmdText :: [String]
invalidCmdText =
  [ "Invalid command"
  ]

readCommand :: IO String
readCommand = do
  putStr "> "
  hFlush stdout
  getLine

handleCommand :: GameState -> String -> (GameState, [String])
handleCommand state cmd = commandToFunction cmd state

commandToFunction :: String -> (GameState -> (GameState, [String]))
commandToFunction cmd
  | cmd `elem` ["n", "north"] = move North
  | cmd `elem` ["s", "south"] = move South
  | cmd `elem` ["e", "east"]  = move East
  | cmd `elem` ["w", "west"]  = move West
  | cmd == "instructions"     = help
  | otherwise                 = invalidCmd

move :: Direction -> GameState -> (GameState, [String])
move dir state = (state, ["Moved " ++ show dir])

help :: GameState -> (GameState, [String])
help state = (state, helpText)

invalidCmd :: GameState -> (GameState, [String])
invalidCmd state = (state, invalidCmdText)
