import Core (gameLoop)
import Console (printLines)
import Data (splashText, menuText, initialGameState)

main = do
  printLines splashText
  printLines menuText
  gameLoop initialGameState
