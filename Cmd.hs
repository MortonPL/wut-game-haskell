module Cmd 
    (
        readCommand
    ) where

readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs
