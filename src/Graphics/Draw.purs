module Graphics.Draw where

import Extra.Prelude

import Graphics.Render
  ( Context
  , clear
  , drawLinesToGrid
  , drawSpriteToGrid
  , drawDotsToGrid
  )
import Types
  ( GameState (..)
  , UIState (..)
  , getTerrainSprite
  , Terrain
  , getEntityType
  )
import Constants (white)
import Entity (spriteAttribute)
import Data.Bimap as Bimap
import GameState (Transformation(..), getEntityPosition)

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
  drawGrowth ctx gs

drawTerrain :: Context -> Terrain -> Effect Unit
drawTerrain ctx = traverseWithIndex_ $ \pos terrainType ->
    drawSpriteToGrid ctx (getTerrainSprite terrainType) pos

drawEntities :: Context -> GameState -> Effect Unit
drawEntities ctx g@(GameState gs) =
  traverseWithIndex_ f (Bimap.leftMap gs.positions)
  where
  f entityId pos =
    let et = getEntityType entityId g
     in case spriteAttribute et of
        Just sprite -> drawSpriteToGrid ctx sprite pos
        Nothing -> pure unit

drawGrowth :: Context -> GameState -> Effect Unit
drawGrowth ctx g@(GameState gs) = 
  traverseWithIndex_ f gs.transformations
  where 
  f i t@(Transformation trans) =
    let pos = getEntityPosition trans.id g
        dots = trans.duration - trans.progress
     in case pos of
       Nothing -> pure unit
       Just p -> drawDotsToGrid ctx dots trans.duration p
   

