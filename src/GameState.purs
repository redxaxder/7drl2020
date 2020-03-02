module GameState where

import Extra.Prelude

import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.Map as Map
import Data.Position (Position)
import Data.Terrain (Terrain, initTerrain)
import Entity (EntityType (..), EntityId (..), increment)
import Random (Gen)

newtype GameState = GameState
  { player :: EntityId
  , terrain :: Terrain
  , positions :: Bimap EntityId Position
  , entities :: Map EntityId EntityType
  , nextEntityId :: EntityId
  , rng :: Gen
  }

newtype EntityConfig = EntityConfig
  { position :: Maybe Position
  , entityType :: EntityType 
  }

newGameState :: Gen -> GameState
newGameState rng =
  let playerId = EntityId 0
  in GameState
     { player: playerId
     , positions: Bimap.singleton playerId (V{x:5,y:5})
     , terrain: initTerrain
     , entities:  Map.fromFoldable
         [ Tuple playerId Player
         ]
     , nextEntityId: EntityId 1
     , rng
     }

createEntity :: EntityConfig -> GameState -> GameState
createEntity (EntityConfig ec) (GameState gs) =
  let eid = gs.nextEntityId
   in GameState gs
      { nextEntityId = increment eid
      , positions = case ec.position of
                      Nothing -> gs.positions
                      Just p -> Bimap.insert eid p gs.positions
      , entities = Map.insert eid ec.entityType gs.entities
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

{-
hoistRandom :: Random GameState -> GameState -> GameState
hoistRandom r (GameState {rng}) =
  let {GameState result, nextGen} = runRandom r rng
   in GameState $ result {rng = nextGen}

      -}
