module Data
  ( initialGameState
  , worldMap
  , helpText
  , invalidCmdText
  , splashText
  , menuText
  )
where

import Types (GameState (..), Player (..), Level (..), Tile (..), Merchant (..))

splashText :: [String]
splashText =
  [ "+=======================================+"
  , "|         'AHOY' IS FIVE DOLLARS        |"
  , "|     A text adventure trading game.    |"
  , "+=======================================+"
  , ""
  , "              |    |    |  "
  , "             )_)  )_)  )_)"
  , "            )___))___))___)"
  , "           )____)____)_____)"
  , "         _____|____|____|________"
  , "~~~~~~~~~\\   lvl 1 crook   \\   /~~~~~~~~~"
  , "  ^^~^^ ^^^~~~~^^^^^~~^^^^^^^    ^^~~^~"
  , "    ^^^  ~~   ^^^^     ^^~    ~~~~"
  , " ~~      ~~^^      ^~^     ~~         ^~^"
  , ""
  , "        by B.Moroz,J.Motyka,D.Sygocki, 2022"
  , ""
  ]

menuText :: [String]
menuText =
  [ "    start - Start a new game."
  , "    save  - Make a new save file."
  , "    load  - Load a saved file."
  , "    quit  - Exit the game."
  ]

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

worldMap :: Level
worldMap = Level
  { size = (6, 6)
  , tiles =
    [ [ShallowWater,   ShallowWater,   ShallowWater,   Island "Northstable Island" (Merchant "Morshu"),       ShallowWater,   ShallowWater]
    , [ShallowWater,   DeepWater,      DeepWater,      DeepWater,      DeepWater,    ShallowWater]
    , [ShallowWater,   DeepWater,      Island "Storm Reef" (Merchant "Tem"),      DeepWater,    DeepWater,    ShallowWater]
    , [Island "White Tiger Island" (Merchant "Daniel Jacks"),     DeepWater,      DeepWater,    DeepWater,    DeepWater,      ShallowWater]
    , [ShallowWater,   DeepWater,      DeepWater,      DeepWater,      DeepWater,    ShallowWater]
    , [ShallowWater,   ShallowWater,   ShallowWater,   ShallowWater,   Island "Bishop Rock Island" (Merchant "Fred"),         ShallowWater]
    ]
  }

initialGameState :: GameState
initialGameState =
  GameState
    {
      player =
        Player
          { name = "hahe",
            position = (0, 0)
          }
    }