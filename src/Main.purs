module Main where

import Extra.Prelude

import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)

import FRP.Event.Keyboard as Keyboard
import Graphics.Draw (draw)
import Graphics.Render (initCanvas)
import Types
  ( GameState
  , Action
  , UIState (..)
  , Position
  )
import Types as T
import Framework.UI (UI (..))
import Framework.Engine (runEngine)

main :: Effect Unit
main = unsafePartial $ launchAff_ $ do
  -- initialize canvas
  Just ctx <- initCanvas { canvasId: "game", spritesheetPath: "tileset.png" }
  let engineConfig =
          { inputs: Keyboard.down
          , ui: uiInit
          , init: init
          , step: update
          , ctx
          , draw
          }
  -- run engine
  cancel <- liftEffect $ runEngine engineConfig
  pure unit

init :: GameState
init = { player: V{x: 3, y:3}}

uiInit :: T.UIAwaitingInput
uiInit = { uiRender: StartScreen, next: \_ -> UIAwaitingInput uiInit }

update :: GameState -> Action -> Maybe GameState
update gs a = stepEnvironment <$> handleAction gs a

stepEnvironment :: GameState -> GameState
stepEnvironment gs = gs

handleAction :: GameState -> Action -> Maybe GameState
handleAction gs _ = Just gs

passable :: GameState -> Position -> Boolean
passable gs pos = true

