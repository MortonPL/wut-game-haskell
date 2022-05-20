module Printer where

-- [HELPER] - Displays a list of strings.
printLines :: [String] -> IO ()
printLines strs = putStr (unlines strs)

-- [HELPER] - Displays a single string.
println :: String -> IO ()
println str = printLines [str]

-- [HELPER] - Displays the spash screen.
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
        "~~~~~~~~~\\ lvl 2 scallywag \\   /~~~~~~~~~",
        "  ^^~^^ ^^^~~~~^^^^^~~^^^^^^^    ^^~~^~",
        "    ^^^  ~~   ^^^^     ^^~    ~~~~",
        " ~~      ~~^^      ^~^     ~~         ^~^",
        "",
        "        by B.Moroz,J.Motyka,D.Sygocki, 2022",
        ""
      ]

-- [HELPER] - Displays the main menu.
menu :: IO ()
menu = printLines menuScreen
  where
    menuScreen =
      [ "    save  - Make a new save file.",
        "    load  - Load a saved file.",
        "    help  - List all commands.",
        "    quit  - Exit the game."
      ]

-- [HELPER] - helper
help :: IO Bool
help = do
  printLines helpScreen
  return True
  where
    helpScreen =
      [ "Available commands:",
        "",
        "h/help        -- helper",
        "l/look        -- look around",
        "n/north       -- go north",
        "e/east        -- go east",
        "s/south       -- go south",
        "w/west        -- go west",
        "i/inventory   -- check inventory",
        "a/appraise    -- appraise items",
        "sell          -- sell items",
        "buy           -- buy items",
        "q/quit        -- end the game and quit",
        ""
      ]
