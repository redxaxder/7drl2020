module Graphics.Draw where

import Extra.Prelude

import Graphics.Render
  ( Context
  , clear
  , drawLinesToGrid
  , drawSpriteToGrid
  , drawGrowthToGrid
  , drawDamageToGrid
  )
import Types
  ( GameState (..)
  , UIState (..)
  , getTerrainSprite
  , Terrain
  , getEntityType
  )
import Constants (white)
import Data.Bimap as Bimap
import Entity (getAttribute)
import GameState (Transformation(..), getEntityPosition)
import Data.Attributes as A

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
  drawDamage ctx gs

drawTerrain :: Context -> Terrain -> Effect Unit
drawTerrain ctx = traverseWithIndex_ $ \pos terrainType ->
    drawSpriteToGrid ctx (getTerrainSprite terrainType) pos

drawEntities :: Context -> GameState -> Effect Unit
drawEntities ctx g@(GameState gs) =
  traverseWithIndex_ f (Bimap.leftMap gs.positions)
  where
  f entityId pos =
    let et = getEntityType entityId g
     in case getAttribute A.sprite et of
        Just sprite -> drawSpriteToGrid ctx sprite pos
        Nothing -> pure unit

drawGrowth :: Context -> GameState -> Effect Unit
drawGrowth ctx g@(GameState gs) = 
  traverse_ f gs.transformations
  where 
  f t@(Transformation trans) =
    let pos = getEntityPosition trans.id g
        dots = trans.duration - trans.progress
     in case pos of
       Nothing -> pure unit
       Just p -> drawGrowthToGrid ctx dots trans.duration p

drawDamage :: Context -> GameState -> Effect Unit
drawDamage ctx g@(GameState gs) = 
  traverseWithIndex_ f gs.hp
  where 
  f eid hp =
    let pos = getEntityPosition eid g
     in case pos of
       Nothing -> pure unit
       Just p -> drawDamageToGrid ctx hp p