module AdventureCore
  ( gameLoop
  )
where

import Cmd
import Control.Monad
import Printer

gameLoop :: GameState -> IO GameState
gameLoop state = do
  cmd <- Cmd.readCommand
  case cmd of
    "quit" -> return state
    _      -> do
      let (newGameState, message) = handleCommand state cmd
      printLines message
      println (show (position (player newGameState)))  -- debug
      gameLoop newGameState
