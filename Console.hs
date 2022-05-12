module Console
  ( printLines,
    println,
    readCommand
  )
where

import System.IO (hFlush, stdout)

printLines :: [String] -> IO ()
printLines strs = putStr (unlines strs)

println :: String -> IO ()
println str = printLines [str]

readCommand :: IO String
readCommand = do
  putStr "> "
  hFlush stdout
  getLine
