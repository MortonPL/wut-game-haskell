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

worldMap :: Level
worldMap = Level
  { size = (6, 6)
  , tiles =
    [ []
    ]
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

movedText :: Direction -> String
movedText dir = "  - Moved " ++ show dir

notMovedText :: Direction -> String
notMovedText dir = "  - No money to be made up " ++ show dir

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

dirToTuple :: Direction -> (Int, Int)
dirToTuple North = (0, -1)
dirToTuple South = (0, 1)
dirToTuple East = (1, 0)
dirToTuple West = (-1, 0)

move :: Direction -> GameState -> (GameState, [String])
move dir state
  | (dir == North && snd (position (player state)) == 0)
    || (dir == South && snd (position (player state)) == snd (size worldMap) - 1)
    || (dir == East && fst (position (player state)) == fst (size worldMap) - 1)
    || (dir == West && fst (position (player state)) == 0)
    = (state, [notMovedText dir])
  | otherwise =
    ( state { player = (player state) { position = position (player state) `add` dirToTuple dir } }
    , [movedText dir]
    )

help :: GameState -> (GameState, [String])
help state = (state, helpText)

invalidCmd :: GameState -> (GameState, [String])
invalidCmd state = (state, invalidCmdText)
