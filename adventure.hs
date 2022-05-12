import AdventureCore
import Printer
import Cmd

initialGameState :: GameState
initialGameState =
  Cmd.GameState
    {
      player =
        Cmd.Player
          { name = "hahe",
            position = (0, 0)
          }
    }

main = do
  Printer.splash
  Printer.menu
  AdventureCore.gameLoop initialGameState
