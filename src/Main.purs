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
  )
import Framework.Engine (runEngine)
import UI (startScreen)
import Direction (move)
import GameState (newGameState, playerPosition, placeEntity, getPlayer, tick, getOccupant)
import GameState as GS
import Data.Attributes as A
import Random (newGen)


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

handleAction :: GameState -> Action -> Maybe GameState
handleAction (GameState {rng}) StartGame = Just $ newGameState rng
handleAction gs (UseItem i) = Just $ execState (GS.consumeItem i) gs
handleAction gs@(GameState g) (Move dir) = flip evalState gs $ do
  -- In this block we have both the gamestate argument (gs) and the
  -- gamestate State available. For reading we use the argument. This represents
  -- how things were at the start of the turn, and this is what checks should
  -- respond to. We use the State when making changes.
  let oldPos = playerPosition gs
      newPos = move dir oldPos
  case getOccupant newPos gs of
    Nothing -> pure unit  -- empty space
    Just occupant -> do
      when (GS.checkEntityAttribute A.item occupant gs) $ -- item
        GS.collectItem occupant
      case GS.getEntityAttribute A.impedes occupant gs of -- tripping obstacle
        Just moveCost -> do
          let mc = if g.noTrip > 0 then 0 else moveCost
          put $ GameState g { noTrip = max 0 $ g.noTrip - 1 }
          modify_ $ GS.alterStamina ((-1) * mc)
        Nothing -> pure unit
  placeEntity (getPlayer gs) newPos
  GS.spawnPlant oldPos
  Just <$> get
handleAction gs (Attack target) = Just
  $ GS.alterStamina (-1) <<< GS.doPlayerAttack target
  $ gs
