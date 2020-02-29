module Graphics.Draw where

import Extra.Prelude

import Graphics.Render
  ( Context
  , clear
  , drawLinesToGrid
  , drawTextToGrid
  )
import Types
  ( GameState (..)
  , UIState (..)
  , Position
  )
import Constants (white)

draw :: Context -> UIState -> GameState -> Effect Unit
draw ctx uiState gs = do
  clear ctx
  case uiState of
       StartScreen -> drawStartScreen ctx
       MainGame -> drawMainGame ctx gs

drawStartScreen :: Context -> Effect Unit
drawStartScreen ctx =
  drawLinesToGrid ctx white (V {x: 16, y: 10})
    [ "Press any key to start" ]

drawMainGame :: Context -> GameState -> Effect Unit
drawMainGame ctx (GameState {player}) =
  drawPlayer ctx player

drawPlayer :: Context -> Position -> Effect Unit
drawPlayer ctx p = drawTextToGrid ctx white "@" p

