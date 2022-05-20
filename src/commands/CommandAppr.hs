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
  r <- randInt (0, 2)
  case r of
    0 -> liftIO $ println "There is nothing to appraise."
    1 -> liftIO $ println $ "There is no `" ++ item ++ "` in your pocket."
    _ -> liftIO $ println $ "You don't own `" ++ item ++ "`. Whetever it may be."
  return ()

describeItem :: String -> StateT GameState IO ()
describeItem item = do
  case item of
    "rum" -> liftIO $ println "Would be delicious with Cola, if I knew what that was."
    "gunpowder" -> liftIO $ println "Let's hope it doesn't explode."
    "blunderbuss" -> liftIO $ println "I like to call it the Blunderbussy."
    "ration" -> liftIO $ println "Meat so dry you can rip pants with it."
    "banana" -> liftIO $ println "A source of comedy. Especially when rotated."
    "map_piece_1" -> liftIO $ println "A map piece that appears to be torn in half. Maybe I can find the other piece if I ask merchants?"
    "map_piece_2" -> liftIO $ println "A map piece that appears to be torn in half. Maybe the pirates know something about the other piece?"
    "mercenary" -> liftIO $ println "The power of Sun Tzu in the palm of your hand. Reduces pirate attacks and deflects them."
    "coin" -> liftIO $ println "Local currency. I think it's named 'Kromer'?"
    _ -> liftIO $ println "-unreachable-entry-"
  return ()
