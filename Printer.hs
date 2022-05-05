module Printer
    (
        splash,
        menu,
        help,
        println
    ) where

printLines :: [String] -> IO ()
printLines strs = putStr (unlines strs)

println str = printLines [str, ""]

splash = printLines splashScreen
    where
        splashScreen =
            [
            "+=======================================+",
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

menu = printLines menuScreen
    where
        menuScreen =
            [
            "    start - Start a new game.",
            "    save  - Make a new save file.",
            "    load  - Load a saved file.",
            "    quit  - Exit the game."
            ]

help = printLines helpScreen
    where
        helpScreen =
            [
            "Available commands are:",
            "",
            "instructions  -- to see these instructions.",
            "quit          -- to end the game and quit.",
            ""
            ]
