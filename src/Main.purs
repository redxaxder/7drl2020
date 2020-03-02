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
import GameState (newGameState, playerPosition, placeEntity, getPlayer, EntityConfig(..), createEntity, tickTransformations, hoist, addTransformation, Transformation (..), doAttack)
import Random (newGen, element)

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray

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
update gs a = stepEnvironment <$> handleAction gs a

stepEnvironment :: GameState -> GameState
stepEnvironment = tickTransformations

spawnOptions :: NonEmptyArray (Tuple EntityType Int)
spawnOptions = NonEmptyArray.cons'
  ( Tuple Grass 1 )
  [ Tuple Tree 6
  ]

spawnPlant :: Position -> State GameState Unit
spawnPlant p =  do
  (Tuple into duration) <- hoist $ element spawnOptions
  id <- createEntity (EntityConfig { position: Just p, entityType: Seed })
  hoist $ addTransformation $ Transformation
    { id
    , into
    , progress: -1
    , duration
    }

handleAction :: GameState -> Action -> Maybe GameState
handleAction (GameState {rng}) StartGame = Just $ newGameState rng
handleAction gs (Move dir) = flip evalState gs $ do
  let oldPos = playerPosition gs
      newPos = move dir oldPos
  placeEntity (getPlayer gs) newPos
  spawnPlant oldPos
  Just <$> get
handleAction gs (Attack target) = Just $ doAttack target gs
