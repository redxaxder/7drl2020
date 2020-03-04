module Data.Terrain where

import Extra.Prelude

import Data.Sprite (Sprite, spriteAt)
import DimArray (Dim (..))

import Data.Typelevel.Num.Reps (D6)

type Terrain = Dim D6 D6 Array TerrainType
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
    [ Dirt, Dirt, Dirt,  Dirt, Dirt, Dirt
    , Dirt, Dirt, Rocks, Dirt, Dirt, Dirt
    , Dirt, Dirt, Dirt,  Dirt, Dirt, Dirt
    , Dirt, Dirt, Dirt,  Dirt, Dirt, Dirt
    , Dirt, Dirt, Dirt,  Dirt, Dirt, Dirt
    , Dirt, Dirt, Dirt,  Dirt, Dirt, Dirt
    ]
