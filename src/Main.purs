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
import Data.Terrain(initTerrain)

main :: Effect Unit
main = unsafePartial $ launchAff_ $ do
  -- initialize canvas
  Just ctx <- initCanvas { canvasId: "game", spritesheetPath: "tileset.png" }
  let engineConfig =
          { inputs: Keyboard.down
          , ui: startScreen
          , init: init
          , step: update
          , ctx
          , draw
          }
  -- run engine
  cancel <- liftEffect $ runEngine engineConfig
  pure unit

init :: GameState
init = GameState
  { player: V{x: 3, y:3}
  , terrain: initTerrain
  }

update :: GameState -> Action -> Maybe GameState
update gs a = stepEnvironment <$> handleAction gs a

stepEnvironment :: GameState -> GameState
stepEnvironment gs = gs

handleAction :: GameState -> Action -> Maybe GameState
handleAction _ StartGame = Just init
handleAction (GameState gs@{player}) (Move dir) =
  Just $ GameState $ gs{ player = move dir player }

