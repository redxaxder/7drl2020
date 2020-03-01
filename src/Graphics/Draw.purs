module Graphics.Draw where

import Extra.Prelude

import Graphics.Render
  ( Context
  , clear
  , drawLinesToGrid
  , drawSpriteToGrid
  )
import Types
  ( GameState (..)
  , UIState (..)
  , Position
  , getTerrainSprite
  , Terrain
  )
import Constants (white)
import Data.Sprite as Sprite

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
drawMainGame ctx (GameState {player, terrain}) = do
  drawTerrain ctx terrain
  drawPlayer ctx player

drawTerrain :: Context -> Terrain -> Effect Unit
drawTerrain ctx = traverseWithIndex_ $ \pos terrainType ->
    drawSpriteToGrid ctx (getTerrainSprite terrainType) pos

drawPlayer :: Context -> Position -> Effect Unit
drawPlayer ctx p = drawSpriteToGrid ctx Sprite.player p

