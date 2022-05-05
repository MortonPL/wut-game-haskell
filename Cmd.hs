module Cmd 
    (
        readCommand
    ) where

readCommand :: IO String
readCommand = do
    putStr "> "
    getLine
