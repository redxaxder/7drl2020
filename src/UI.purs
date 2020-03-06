module UI where

import Framework.UI (UI (..))
import Types as T
import Types (UIState (..), Action (..), Key, GameState (..), playerPosition, hasAttribute)
import Direction (Direction (..), move)
import DimArray (index)
import Extra.Prelude
import Data.Terrain (blocksMovement)
import GameState (getEntityType, getOccupant)
import Entity (getAttribute)
import Data.Attributes as A

-- Javascript key codes here: https://keycode.info/

startScreen :: T.UIAwaitingInput
startScreen =
  { uiRender: StartScreen
  , next: \_ -> run StartGame
  }

gameOverScreen :: T.UIAwaitingInput
gameOverScreen = 
  { uiRender: GameOverScreen
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
    next "KeyD"       = die gs
    next _            = runningGameUI gs

chooseSensibleAction :: GameState -> Direction -> T.UI
chooseSensibleAction g@(GameState gs) dir =
  case { canAttack, terrainBlocking, blocking, tooImpeding } of
    { terrainBlocking: true } -> runningGameUI g
    { canAttack: Just id } ->
      if gs.stamina > 0
        then run $ Attack id
        else runningGameUI g
    { blocking: false, tooImpeding: false } -> run $ Move dir
    _otherwise -> runningGameUI g
  where
  target = move dir (playerPosition g)
  occupant = getOccupant target g
  occupantType = flip getEntityType g <$> occupant
  terrainBlocking = fromMaybe true $ blocksMovement <$> index gs.terrain target
  blocking = fromMaybe false $ hasAttribute A.blocking <$> occupantType
  tooImpeding = fromMaybe false $
    (getAttribute A.impedes =<< occupantType)
    <#> \reqStamina -> reqStamina > gs.stamina
  canAttack = do
    guard =<< (hasAttribute A.attackable <$> occupantType)
    occupant

die :: GameState -> T.UI
die _ = UIAwaitingInput gameOverScreen
