module Random where

import Control.Monad.State
  ( StateT,
  )
import DataTypes (GameState)
import System.Random (randomRIO)

-- [HELPER] - Random int from provided range.
randInt :: (Int, Int) -> StateT GameState IO Int
randInt randRange = do
  randomRIO randRange

-- [HELPER] - Random float from 0-1 range.
randFloat :: StateT GameState IO Float
randFloat = do
  randomRIO (0.0, 1.0)
