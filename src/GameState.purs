module GameState where

import Extra.Prelude

import Data.Bimap (Bimap)
import Data.Array as Array
import Data.Bimap as Bimap
import Data.Map as Map
import Data.Position (Position)
import Data.Terrain (Terrain, initTerrain)
import Entity (EntityType (..), EntityId (..), increment)
import Random (Gen, Random, runRandom)

newtype GameState = GameState
  { player :: EntityId
  , terrain :: Terrain
  , positions :: Bimap EntityId Position
  , entities :: Map EntityId EntityType
  , nextEntityId :: EntityId
  , rng :: Gen
  , transformations :: Array Transformation
  }

newtype Transformation = Transformation
  { id :: EntityId
  , into :: EntityType
  , progress :: Int
  , duration :: Int
  }

addTransformation :: Transformation -> GameState -> GameState
addTransformation t (GameState gs) = GameState $
  gs { transformations = Array.cons t gs.transformations }

transform :: EntityId -> EntityType -> GameState -> GameState
transform id into (GameState gs) =
  GameState $ gs { entities = Map.insert id into gs.entities }

tickTransformations :: GameState -> GameState
tickTransformations (GameState gs) =
  let {yes, no} = Array.partition ready (tick <$> gs.transformations)
      Endo f = foldMap Endo (apply <$> yes)
   in f $ GameState $ gs { transformations = no }
   where
   ready (Transformation { progress, duration }) = progress >= duration
   apply (Transformation { id, into }) = transform id into
   tick (Transformation t) = Transformation $ t{ progress = t.progress + 1 }

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
     , transformations: mempty
     }

createEntity :: EntityConfig -> State GameState EntityId
createEntity (EntityConfig ec) = do
  GameState {nextEntityId} <- get
  modify_ $ \(GameState gs) -> GameState gs
      { nextEntityId = increment nextEntityId
      , positions = case ec.position of
                      Nothing -> gs.positions
                      Just p -> Bimap.insert nextEntityId p gs.positions
      , entities = Map.insert nextEntityId ec.entityType gs.entities
      }
  pure nextEntityId

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

hoistRandom :: forall a. Random a -> State GameState a
hoistRandom r = do
  GameState {rng} <- get
  let {result, nextGen} = runRandom r rng
  modify_ $ \(GameState gs) -> GameState $ gs {rng = nextGen}
  pure result

