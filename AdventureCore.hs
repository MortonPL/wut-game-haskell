module AdventureCore
  ( gameLoop
  )
where

import Cmd
import Control.Monad
import Printer

-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: GameState -> IO GameState
gameLoop state = do
  cmd <- Cmd.readCommand
  case cmd of
    "quit" -> return state
    _      -> do
      let (newGameState, message) = handleCommand state cmd
      printLines message
      gameLoop newGameState
