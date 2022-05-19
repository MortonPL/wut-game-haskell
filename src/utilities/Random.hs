module Random where

import Control.Monad.State
  ( StateT,
  )
import DataTypes (GameState)
import System.Random (randomRIO)

randInt :: (Int, Int) -> StateT GameState IO Int
randInt randRange = do
  randomRIO randRange

randFloat :: StateT GameState IO Float
randFloat = do
  randomRIO (0.0, 1.0)
