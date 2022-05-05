module AdventureCore
    (
        gameLoop
    ) where

import Cmd
import Printer

-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: IO ()
gameLoop = do
    cmd <- Cmd.readCommand
    case cmd of
        "instructions" -> do Printer.help
                             gameLoop
        "quit" -> return ()
        _ -> do Printer.println "Unknown command."
                gameLoop
