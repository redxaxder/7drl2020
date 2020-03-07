module Data.Terrain where

import Extra.Prelude

import Data.Sprite (Sprite, spriteAt)
import DimArray (Dim (..))
import DimArray as DimArray
import Data.Array as Array
import Data.Position (Position)
import Data.Typelevel.Num.Reps (D6)

import Random (Random)
import Random as Random

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
initTerrain pos = unsafeFromJust $ DimArray.updateAt pos House $
  Dim $ Array.replicate 36 (Dirt 0)

nextTerrain :: TerrainType -> Tuple Int TerrainType
nextTerrain House = Tuple 0 House
nextTerrain (Dirt i) = case i of
  0 -> Tuple 30 (Dirt 1)
  1 -> Tuple 15 (Dirt 2)
  2 -> Tuple 5  (Dirt 3)
  _ -> Tuple 0  (Dirt 3)

advanceTerrain :: Position -> Terrain -> Random Terrain
advanceTerrain p terrain =
  let t = nextTerrain <$> DimArray.index terrain p in
  case t of
    Nothing -> pure terrain
    Just (Tuple chance next) -> do
      doAdvancement <- Random.chance chance
      pure $ if doAdvancement
        then fromMaybe terrain $ DimArray.updateAt p next terrain
        else terrain
