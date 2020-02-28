module Graphics.Draw where

import Extra.Prelude

import Graphics.Render
  ( Context
  , clear
  )
import Types
  ( GameState
  , UIState (..)
  )

draw :: Context -> UIState -> GameState -> Effect Unit
draw ctx uiRenderData gs = do
  clear ctx
  case uiRenderData of
       StartScreen -> pure unit
       _ -> pure unit
