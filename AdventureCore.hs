module AdventureCore
  ( gameLoop,
  )
where

import Cmd
import Control.Monad
import Printer

-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: IO ()
gameLoop = do
  continue <- Cmd.handleCommand
  when continue gameLoop
