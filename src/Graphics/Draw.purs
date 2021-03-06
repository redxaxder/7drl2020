module Graphics.Draw where

import Extra.Prelude

import Graphics.Render
  ( Context
  , clear
  , drawSpriteToGrid
  , drawGrowthToGrid
  , drawDamageToGrid
  )
import Types
  ( GameState (..)
  , UIState (..)
  , getTerrainSprite
  , Terrain
  , unsafeGetEntityType
  )
import Data.Bimap as Bimap
import Entity (getAttribute)
import GameState (Transformation(..), getEntityPosition, getEntityAttribute)
import Data.Attributes as A
import Data.Sprite as Sprite
import Data.Array as Array
import Data.String.CodeUnits as String
import Data.Int as Int

type Shift = Vector Int

mainShift :: Shift
mainShift = V {x:1,y:1}

draw :: Context -> UIState -> GameState -> Effect Unit
draw ctx uiState gs = do
  clear ctx
  case uiState of
       StartScreen -> drawStartScreen ctx
       MainGame -> drawMainGame ctx gs
       GameOverScreen -> drawGameOverScreen ctx gs

drawTextSprites :: Context -> Shift -> String -> Effect Unit
drawTextSprites ctx shift str =
  forWithIndex_ (Sprite.letterSprite <$> String.toCharArray str) \x s ->
    drawSpriteToGrid ctx s (V{x,y:0} + shift)



drawStartScreen :: Context -> Effect Unit
drawStartScreen ctx = forWithIndex_
  [ "        "
  , "Press   "
  , "        "
  , "any key "
  , "        "
  , "to start"
  , "        "
  , "        "
  ] \y str -> drawTextSprites ctx (V{x:0,y}) str

drawGameOverScreen :: Context -> GameState -> Effect Unit
drawGameOverScreen ctx (GameState {score}) = do
  forWithIndex_
    [ "        "
    , " Final  "
    , "        "
    , " score  "
    , "        "
    , "   000  "
    , "        "
    , "        "
    ] \y str -> drawTextSprites ctx (V{x:0,y}) str
  drawScore ctx (V{x:3,y:5}) score

drawMainGame :: Context -> GameState -> Effect Unit
drawMainGame ctx gs@(GameState { terrain
                               , stamina
                               , attackBuff
                               , score
                               , noTrip
                               , timeFreeze
                               , onlyGrass}) = do
  drawStamina ctx (V {x:1,y:0}) stamina
  drawTerrain ctx mainShift terrain
  drawEntities ctx mainShift gs
  drawGrowth ctx mainShift gs
  drawDamage ctx mainShift gs
  drawInventory ctx (V {x:1,y:7}) gs
  drawAttackUp ctx (V {x:7,y:0}) attackBuff
  drawNoTrip ctx (V {x:0,y:0}) noTrip
  drawTimeFreeze ctx (V {x:0,y:4}) timeFreeze
  drawOnlyGrass ctx (V {x:7, y:4}) onlyGrass
  drawScore ctx (V {x:4, y:7}) score

drawTerrain :: Context -> Shift -> Terrain -> Effect Unit
drawTerrain ctx shift = traverseWithIndex_ $ \pos terrainType ->
    drawSpriteToGrid ctx (getTerrainSprite terrainType) (pos + shift)

drawStamina :: Context -> Shift -> Int -> Effect Unit
drawStamina ctx shift stamina =
  for_ (Array.range 0 5) \x ->
    let s = if stamina > x then Sprite.stamina else Sprite.blank
     in drawSpriteToGrid ctx s (V {x,y:0} + shift)

drawAttackUp :: Context -> Shift -> Int -> Effect Unit
drawAttackUp ctx shift attackUp = 
  for_ (Array.range 0 3) \y ->
    let s = if attackUp > y then Sprite.attackUp else Sprite.blank
     in drawSpriteToGrid ctx s (V {x:0,y} + shift)

drawNoTrip :: Context -> Shift -> Int -> Effect Unit
drawNoTrip ctx shift noTrip =
  for_ (Array.range 0 3) \y ->
    let s = if noTrip > y then Sprite.noTrip else Sprite.blank
      in drawSpriteToGrid ctx s (V {x:0, y} + shift)

drawTimeFreeze :: Context -> Shift -> Int -> Effect Unit
drawTimeFreeze ctx shift timeFreeze =
  for_ (Array.range 0 3) \y ->
    let s = if timeFreeze > y then Sprite.timeFreeze else Sprite.blank
      in drawSpriteToGrid ctx s (V {x:0, y} + shift)

drawOnlyGrass :: Context -> Shift -> Int -> Effect Unit
drawOnlyGrass ctx shift onlyGrass =
  for_ (Array.range 0 3) \y ->
    let s = if onlyGrass > y then Sprite.onlyGrass else Sprite.blank
      in drawSpriteToGrid ctx s (V {x:0, y} + shift)

drawInventory :: Context -> Shift -> GameState -> Effect Unit
drawInventory ctx shift gs@(GameState {inventory}) = do
  for_ (Array.range 0 2) \i ->
     let sprite = fromMaybe Sprite.blank
           $ Array.index inventory i >>= \id
           -> getEntityAttribute A.sprite id gs
      in do
        drawSpriteToGrid ctx Sprite.blank (V{x: i, y: 0} + shift)
        drawSpriteToGrid ctx sprite (V{x: i, y: 0} + shift)

digits :: Int -> Array Int
digits n = n # show
  >>> String.toCharArray
  >>> map String.singleton
  >>> map Int.fromString
  >>> Array.catMaybes
  >>> append [0,0,0]
  >>> Array.takeEnd 3

drawScore :: Context -> Shift -> Int -> Effect Unit
drawScore ctx shift score =
  let ds = digits score
   in forWithIndex_ ds \i d ->
        drawSpriteToGrid ctx (Sprite.digitSprite d) (V{x:i, y:0} + shift)

drawEntities :: Context -> Shift -> GameState -> Effect Unit
drawEntities ctx shift g@(GameState gs) =
  traverseWithIndex_ f (Bimap.leftMap gs.positions)
  where
  f entityId pos =
    let et = unsafeGetEntityType entityId g
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
