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

-- [HELPER] - helper
help :: IO Bool
help = do
  printLines helpScreen
  return True
  where
    helpScreen =
      [ "Available commands:",
        "",
        "[H]elp        -- to see this list.",
        "[L]ook        -- look around",
        "[I]nventory   -- check inventory",
        "[A]ppraise    -- appraise items",
        "[N]orth       -- go north.",
        "[E]ast        -- \" east.",
        "[S]outh       -- \" south.",
        "[W]est        -- \" west.",
        "[B]uy         -- buy items",
        "[Z]sell       -- sell items",
        "as[K]         -- ask quesitons",
        "sa[V]e <file> -- save the game.",
        "loa[D] <file> -- load the game.",
        "[Q]uit        -- end the game and quit.",
        ""
      ]
