module Random where

import Control.Monad.State
  ( StateT,
  )
import System.Random (randomRIO)

import DataTypes (GameState)

randInt :: (Int, Int) -> StateT GameState IO Int
randInt randRange = do
  randomRIO randRange
