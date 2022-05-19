module GameState where

import DataTypes (Player (Player, pl_name, pl_position))

initGameState :: Player
initGameState =
  Player
    { pl_name = "joe",
      pl_position = (0, 0)
    }
