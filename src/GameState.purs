module GameState where

import Extra.Prelude

import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.Map as Map
import Data.Position (Position)
import Data.Terrain (Terrain, initTerrain)
import Entity (EntityType (..), EntityId (..))

newtype GameState = GameState
  { player :: EntityId
  , terrain :: Terrain
  , positions :: Bimap EntityId Position
  , entities :: Map EntityId EntityType
  }

init :: GameState
init =
  let playerId = EntityId 0
      grassId = EntityId 1
      grassPos = V {x:4,y:4}
  in GameState
     { player: playerId
     , positions: Bimap.insert grassId grassPos $ Bimap.singleton playerId (V{x:5,y:5})
     , terrain: initTerrain
     , entities:  Map.fromFoldable
         [ Tuple playerId Player
         , Tuple grassId Grass
         ]
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
placeEntity eid pos (GameState gs) = 
  let newp = Bimap.insert eid pos gs.positions
   in GameState $ gs { positions = newp }



