module GameState where

import Extra.Prelude

import Data.Attribute (Attribute (..))
import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.Map as Map
import Data.Position (Position)
import Data.Sprite (Sprite (..), spriteAt)
import Data.Terrain (Terrain, TerrainType (..), getTerrainSprite, initTerrain)
import Direction (Direction)
import Entity (EntityType (..), EntityId (..))


newtype GameState = GameState
  { player :: EntityId
  , terrain :: Terrain
  , positions :: Bimap EntityId Position
  , entities :: Map EntityId EntityType
  }

init :: GameState
init = let playerId = EntityId 0
  in GameState
     { player: playerId
     , positions: Bimap.singleton playerId (V{x:5,y:5})
     , terrain: initTerrain
     , entities:  Map.singleton playerId Player
     }

getPlayer :: GameState -> EntityId
getPlayer (GameState {player}) = player

getEntityPosition :: EntityId -> GameState -> Maybe Position
getEntityPosition eid (GameState {positions}) = Bimap.lookup eid positions

playerPosition :: GameState -> Position
playerPosition gs = unsafeFromJust $ getEntityPosition (getPlayer gs) gs

getEntityType :: EntityId -> GameState -> EntityType
getEntityType eid (GameState {entities}) = unsafeFromJust $ Map.lookup eid entities

placeEntity :: EntityId -> Position -> GameState -> GameState
placeEntity eid pos (GameState gs) = GameState $ gs
  { positions = Bimap.insert eid pos gs.positions
  }



