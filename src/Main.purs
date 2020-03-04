module Main where

import Extra.Prelude

import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)

import FRP.Event.Keyboard as Keyboard
import Graphics.Draw (draw)
import Graphics.Render (initCanvas)
import Types
  ( GameState (..)
  , Action (..)
  , EntityType (..)
  , Position
  )
import Framework.Engine (runEngine)
import UI (startScreen)
import Direction (move)
import GameState (newGameState, playerPosition, placeEntity, getPlayer, EntityConfig(..), createEntity, tick, hoist, addTransformation, Transformation (..), doAttack, getOccupant)
import GameState as GS
import Data.Attributes as A
import Random (newGen, element)
import Entity (entitiesWithAttribute)


main :: Effect Unit
main = unsafePartial $ launchAff_ $ do
  -- initialize canvas
  Just ctx <- initCanvas { canvasId: "game", spritesheetPath: "tileset.png" }
  init <- liftEffect $ newGameState <$> newGen
  let engineConfig =
          { inputs: Keyboard.down
          , ui: startScreen
          , init
          , step: update
          , ctx
          , draw
          }
  -- run engine
  cancel <- liftEffect $ runEngine engineConfig
  pure unit

update :: GameState -> Action -> Maybe GameState
update gs a = tick <$> handleAction gs a

spawnPlant :: Position -> State GameState Unit
spawnPlant p =  do
  (Tuple {entityType} duration) <- hoist $ element (entitiesWithAttribute A.plant)
  id <- createEntity (EntityConfig { position: Just p, entityType: Seed })
  hoist $ addTransformation $ Transformation
    { id
    , into: entityType
    , progress: -1
    , duration
    }

handleAction :: GameState -> Action -> Maybe GameState
handleAction (GameState {rng}) StartGame = Just $ newGameState rng
handleAction gs (Move dir) = flip evalState gs $ do
  let oldPos = playerPosition gs
      newPos = move dir oldPos
      staminaChange = fromMaybe 0 $ do
        occupant <- getOccupant newPos gs
        cost <- GS.getEntityAttribute A.impedes occupant gs
        pure $ -1 * cost
  modify_ $ GS.alterStamina staminaChange
  placeEntity (getPlayer gs) newPos
  spawnPlant oldPos
  Just <$> get
handleAction gs (Attack target) = Just
  $ GS.alterStamina (-1) <<< doAttack target
  $ gs
