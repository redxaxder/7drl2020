module GameState where

import Extra.Prelude

import Data.Bimap (Bimap)
import Data.Array as Array
import Data.Bimap as Bimap
import Data.Map as Map
import Data.Position (Position)
import Data.Terrain (Terrain, initTerrain, TerrainType)
import Entity (EntityType (..), EntityId (..), increment)
import Random (Gen, Random, runRandom)
import DimArray (Dim)

import Data.Lens.Zoom (zoom)
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy (..))
import Data.Typelevel.Num.Reps (D8)


newtype GameState = GameState
  { player :: EntityId
  , terrain :: Terrain
  , positions :: Bimap EntityId Position
  , entities :: Map EntityId EntityType
  , nextEntityId :: EntityId
  , rng :: Gen
  , transformations :: Array Transformation
  }

derive instance newtypeGameState :: Newtype GameState _

newtype Transformation = Transformation
  { id :: EntityId
  , into :: EntityType
  , progress :: Int
  , duration :: Int
  }

addTransformation :: Transformation -> State (Array Transformation) Unit
addTransformation t = modify_ $ Array.cons t

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
  GameState { nextEntityId } <- get
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

placeEntity :: EntityId -> Position -> State (Bimap EntityId Position) Unit
placeEntity eid pos = modify_ $ Bimap.insert eid pos

class Hoist m where
  hoist :: forall a. m a -> State GameState a

instance hoistRandom :: Hoist Random where
  hoist r =  do
    GameState {rng} <- get
    let {result, nextGen} = runRandom r rng
    modify_ $ \(GameState gs) -> GameState $ gs {rng = nextGen}
    pure result

instance hoistTerrain :: Hoist (StateT (Dim D8 D8 Array TerrainType) Identity) where
  hoist = zoom $ _Newtype <<< prop (SProxy :: SProxy "terrain")

instance hoistPositions :: Hoist (StateT (Bimap EntityId (Vector Int)) Identity) where
  hoist = zoom $ _Newtype <<< prop (SProxy :: SProxy "positions")

instance hoistEntities :: Hoist (StateT (Map EntityId EntityType) Identity) where
  hoist = zoom $ _Newtype <<< prop (SProxy :: SProxy "entities")

instance hoistTransformations :: Hoist (StateT (Array Transformation) Identity) where
  hoist = zoom $ _Newtype <<< prop (SProxy :: SProxy "transformations")
