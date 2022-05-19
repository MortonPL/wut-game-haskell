module Printer where

printLines :: [String] -> IO ()
printLines strs = putStr (unlines strs)

println :: String -> IO ()
println str = printLines [str]

splash :: IO ()
splash = printLines splashScreen
  where
    splashScreen =
      [ "+=======================================+",
        "|         'AHOY' IS FIVE DOLLARS        |",
        "|     A text adventure trading game.    |",
        "+=======================================+",
        "",
        "              |    |    |  ",
        "             )_)  )_)  )_)",
        "            )___))___))___)",
        "           )____)____)_____)",
        "         _____|____|____|________",
        "~~~~~~~~~\\   lvl 1 crook   \\   /~~~~~~~~~",
        "  ^^~^^ ^^^~~~~^^^^^~~^^^^^^^    ^^~~^~",
        "    ^^^  ~~   ^^^^     ^^~    ~~~~",
        " ~~      ~~^^      ^~^     ~~         ^~^",
        "",
        "        by B.Moroz,J.Motyka,D.Sygocki, 2022",
        ""
      ]

menu :: IO ()
menu = printLines menuScreen
  where
    menuScreen =
      [ "    save  - Make a new save file."
      , "    load  - Load a saved file."
      , "    help  - List all commands."
      , "    quit  - Exit the game."
      ]

help :: IO Bool
help = do
  printLines helpScreen
  return True
  where
    helpScreen =
      [ "Available commands are:"
      , ""
      , "h/help        -- to see this list"
      , "n/north       -- to go north"
      , "e/east        -- \"  \"  east"
      , "s/south       -- \"  \"  south"
      , "w/west        -- \"  \"  west"
      , "sell          -- to sell items"
      , "buy           -- to buy items"
      , "q/quit        -- to end the game and quit"
      , ""
      ]
