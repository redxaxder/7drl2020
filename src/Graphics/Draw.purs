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
import Constants (white, red)
import Data.Bimap as Bimap
import Entity (getAttribute)
import GameState (Transformation(..), getEntityPosition, getEntityAttribute)
import Data.Attributes as A
import Data.Sprite as Sprite
import Data.Array as Array

type Shift = Vector Int

mainShift :: Shift
mainShift = V {x:0,y:1}

inventoryShift :: Shift
inventoryShift = V {x:0,y:7}

staminaShift :: Shift
staminaShift = V {x:0,y:0}
draw :: Context -> UIState -> GameState -> Effect Unit
draw ctx uiState gs = do
  clear ctx
  case uiState of
       StartScreen -> drawStartScreen ctx
       MainGame -> drawMainGame ctx gs
       GameOverScreen -> drawGameOverScreen ctx gs

drawStartScreen :: Context -> Effect Unit
drawStartScreen ctx =
  drawLinesToGrid ctx white (V {x: 9, y: 10})
    [ "Press any key to start" ]

drawGameOverScreen :: Context -> GameState -> Effect Unit
drawGameOverScreen ctx (GameState {score}) = do
  drawLinesToGrid ctx red (V {x: 14, y:10})
    [ "GAME OVER" ]
  drawLinesToGrid ctx white (V {x: 15, y: 12})
    [ "score"
    , show score
    ]

drawMainGame :: Context -> GameState -> Effect Unit
drawMainGame ctx gs@(GameState {terrain, stamina}) = do
  drawStamina ctx staminaShift stamina
  drawTerrain ctx mainShift terrain
  drawEntities ctx mainShift gs
  drawGrowth ctx mainShift gs
  drawDamage ctx mainShift gs
  drawInventory ctx inventoryShift gs

drawTerrain :: Context -> Shift -> Terrain -> Effect Unit
drawTerrain ctx shift = traverseWithIndex_ $ \pos terrainType ->
    drawSpriteToGrid ctx (getTerrainSprite terrainType) (pos + shift)

drawStamina :: Context -> Shift -> Int -> Effect Unit
drawStamina ctx shift stamina =
  for_ (Array.range 0 5) \x ->
    let s = if stamina > x then Sprite.stamina else Sprite.blank
     in drawSpriteToGrid ctx s (V {x,y:0} + shift)

drawInventory :: Context -> Shift -> GameState -> Effect Unit
drawInventory ctx shift gs@(GameState {inventory}) = do
  drawSpriteToGrid ctx Sprite.one (V{x:0,y:0} + shift)
  drawSpriteToGrid ctx Sprite.two (V{x:2,y:0} + shift)
  drawSpriteToGrid ctx Sprite.three (V{x:4,y:0} + shift)
  for_ (Array.range 0 2) \i ->
     let sprite = fromMaybe Sprite.blank
           $ Array.index inventory i >>= \id
           -> getEntityAttribute A.sprite id gs
      in drawSpriteToGrid ctx sprite (V{x: i*2 + 1, y:0} + shift)

drawEntities :: Context -> Shift -> GameState -> Effect Unit
drawEntities ctx shift g@(GameState gs) =
  traverseWithIndex_ f (Bimap.leftMap gs.positions)
  where
  f entityId pos =
    let et = getEntityType entityId g
     in case getAttribute A.sprite et of
        Just sprite -> drawSpriteToGrid ctx sprite (pos + shift)
        Nothing -> pure unit

drawGrowth :: Context -> Shift -> GameState -> Effect Unit
drawGrowth ctx shift g@(GameState gs) =
  traverse_ f gs.transformations
  where
  f t@(Transformation trans) =
    let pos = getEntityPosition trans.id g
        dots = trans.duration - trans.progress
     in case pos of
       Nothing -> pure unit
       Just p -> drawGrowthToGrid ctx dots trans.duration (p + shift)

drawDamage :: Context -> Shift -> GameState -> Effect Unit
drawDamage ctx shift g@(GameState gs) =
  traverseWithIndex_ f gs.hp
  where
  f eid hp =
    let pos = getEntityPosition eid g
     in case pos of
       Nothing -> pure unit
       Just p -> drawDamageToGrid ctx hp (p + shift)
