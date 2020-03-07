module Data.Terrain where

import Extra.Prelude

import Data.Sprite (Sprite, spriteAt)
import DimArray (Dim (..))
import DimArray as DimArray
import Data.Array as Array
import Data.Position (Position)
import Data.Typelevel.Num.Reps (D6)

type Terrain = Dim D6 D6 Array TerrainType
data TerrainType = Dirt Int | House

getTerrainSprite :: TerrainType -> Sprite
getTerrainSprite House = spriteAt 1 20
getTerrainSprite (Dirt 0) = spriteAt 1 0
getTerrainSprite (Dirt 1) = spriteAt 2 0
getTerrainSprite (Dirt 2) = spriteAt 3 0
getTerrainSprite (Dirt _) = spriteAt 4 0

derive instance eqTerrainType :: Eq TerrainType

blocksMovement :: TerrainType -> Boolean
blocksMovement House = true
blocksMovement _ = false

initTerrain :: Position -> Terrain
initTerrain pos = unsafeFromJust $ DimArray.insertAt pos House $
  Dim $ Array.replicate 36 (Dirt 0)
