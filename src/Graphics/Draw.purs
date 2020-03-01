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
  , getTerrainSprite
  , Terrain
  , getEntityType
  )
import Constants (white)
import Entity (lookupEntity)
import Data.Bimap as Bimap

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
drawMainGame ctx gs@(GameState {player, terrain}) = do
  drawTerrain ctx terrain
  drawEntities ctx gs

drawTerrain :: Context -> Terrain -> Effect Unit
drawTerrain ctx = traverseWithIndex_ $ \pos terrainType ->
    drawSpriteToGrid ctx (getTerrainSprite terrainType) pos

drawEntities :: Context -> GameState -> Effect Unit
drawEntities ctx g@(GameState gs) =
  traverseWithIndex_ f (Bimap.leftMap gs.positions)
  where
  f entityId pos =
    let et = getEntityType entityId g
        { sprite } = lookupEntity et
     in drawSpriteToGrid ctx sprite pos
   

