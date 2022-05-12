module Core
  ( gameLoop
  )
where

import Types (GameState (..), Player (..))
import Console (readCommand, printLines, println)
import Cmd (handleCommand)
import Control.Monad

gameLoop :: GameState -> IO GameState
gameLoop state = do
  cmd <- readCommand
  case cmd of
    "quit" -> return state
    _      -> do
      let (newGameState, message) = handleCommand state cmd
      printLines message
      println (show (position (player newGameState)))  -- debug
      gameLoop newGameState
