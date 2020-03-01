module UI where

import Framework.UI (UI (..))
import Types as T
import Types (UIState (..), Action (..), Key, GameState (..), playerPosition)
import Direction (Direction (..), move)
import DimArray (index)
import Extra.Prelude
import Data.Terrain (blocksMovement)

-- Javascript key codes here: https://keycode.info/

startScreen :: T.UIAwaitingInput
startScreen =
  { uiRender: StartScreen
  , next: \_ -> run StartGame
  }

run :: Action -> T.UI
run a = UIAction { uiAction: a, next: runningGameUI }

runningGameUI :: GameState -> T.UI
runningGameUI gs = UIAwaitingInput { uiRender: MainGame, next}
  where
    next :: Key -> T.UI
    next "ArrowLeft"  = chooseSensibleAction gs L
    next "KeyH"       = chooseSensibleAction gs L
    next "ArrowRight" = chooseSensibleAction gs R
    next "KeyL"       = chooseSensibleAction gs R
    next "ArrowDown"  = chooseSensibleAction gs D
    next "KeyJ"       = chooseSensibleAction gs D
    next "ArrowUp"    = chooseSensibleAction gs U
    next "KeyK"       = chooseSensibleAction gs U
    next _            = runningGameUI gs

chooseSensibleAction :: GameState -> Direction -> T.UI
chooseSensibleAction g@(GameState gs) dir =
  if canMove then run (Move dir) else runningGameUI g
  where
    targetTerrain = index gs.terrain $ move dir (playerPosition g)
    canMove = case targetTerrain of
      Nothing -> false
      Just t -> not $ blocksMovement t
