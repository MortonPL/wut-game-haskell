module CommandAppr where

import Control.Monad.State
import DataTypes
import Inventory (count)
import Printer (println)
import Random (randInt)

appraise :: String -> StateT GameState IO Bool
appraise item = do
  state <- get
  let inv = pl_inventory state
  if count inv item /= 0
    then describeItem item
    else missingItem item
  return True

missingItem :: String -> StateT GameState IO ()
missingItem item = do
  r <- randInt (0, 4)
  case r of
    0 -> liftIO $ println $ "What is a `" ++ item ++ "`?"
    1 -> liftIO $ println $ "You must remember `" ++ item ++ "` from your childhood."
    2 -> liftIO $ println $ "PLACEHOLDER " ++ item
    3 -> liftIO $ println $ "PLACEHOLDER " ++ item
    4 -> liftIO $ println $ "PLACEHOLDER " ++ item
    _ -> liftIO $ println $ "DEFAULT PLACEHOLDER " ++ item
  return ()

describeItem :: String -> StateT GameState IO ()
describeItem item = do
  case item of
    "balls" -> liftIO $ println "nuts even, testicles if you may"
    "" -> liftIO $ println ""
    _ -> liftIO $ println "how did you get here??"
  return ()
