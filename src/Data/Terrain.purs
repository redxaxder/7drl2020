module Data.Terrain where

import Extra.Prelude

import Data.Sprite (Sprite, spriteAt)
import DimArray (Dim (..))

import Data.Typelevel.Num.Reps (D8)

type Terrain = Dim D8 D8 Array TerrainType
data TerrainType = Dirt | Rocks

getTerrainSprite :: TerrainType -> Sprite
getTerrainSprite Dirt = spriteAt 2 0
getTerrainSprite Rocks = spriteAt 5 2

derive instance eqTerrainType :: Eq TerrainType

blocksMovement :: TerrainType -> Boolean
blocksMovement Rocks = true
blocksMovement _ = false

initTerrain :: Terrain
initTerrain = Dim $ 
    [ Rocks, Rocks, Rocks, Rocks, Rocks, Rocks, Rocks, Rocks,
      Rocks, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Rocks, 
      Rocks, Dirt, Dirt, Rocks, Dirt, Dirt, Dirt, Rocks, 
      Rocks, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Rocks, 
      Rocks, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Rocks, 
      Rocks, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Rocks, 
      Rocks, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Rocks, 
      Rocks, Rocks, Rocks, Rocks, Rocks, Rocks, Rocks, Rocks ]