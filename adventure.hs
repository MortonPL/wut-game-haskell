import AdventureCore
import Printer
import Cmd

initialGameState :: GameState
initialGameState =
  Cmd.GameState
    { level =
        Cmd.Level
          { size = (6, 6),
            tiles =
              [ []
              ]
          },
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
