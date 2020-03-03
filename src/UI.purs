module UI where

import Framework.UI (UI (..))
import Types as T
import Types (UIState (..), Action (..), Key, GameState (..), playerPosition, hasAttribute)
import Direction (Direction (..), move)
import DimArray (index)
import Extra.Prelude
import Data.Terrain (blocksMovement)
import GameState (getEntityType)
import Data.Bimap as Bimap
import Data.Attributes as A

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
  case terrainBlocking of
       true -> runningGameUI g
       false ->
         case Bimap.lookupR target gs.positions of
              Nothing -> run $ Move dir
              Just id ->
                let et = getEntityType id g
                    blocking = hasAttribute A.blocking et
                    attackable = hasAttribute A.attackable et
                 in case { blocking, attackable } of
                    { attackable: true } -> run (Attack id)
                    { blocking: true } -> runningGameUI g
                    { blocking: false } -> run $ Move dir
  where
    target = move dir (playerPosition g)
    targetTerrain = index gs.terrain target
    terrainBlocking = case targetTerrain of
      Nothing -> true
      Just t -> blocksMovement t
