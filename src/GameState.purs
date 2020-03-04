module GameState where

import Extra.Prelude

import Data.Bimap (Bimap)
import Data.Array as Array
import Data.Bimap as Bimap
import Data.Map as Map
import Data.Position (Position)
import Data.Terrain (Terrain, initTerrain, TerrainType, blocksMovement)
import Entity (EntityType (..), EntityId (..), increment, hasAttribute, getAttribute, hasFlag, entitiesWithAttribute)
import Random (Gen, Random, runRandom, intRange, element)
import DimArray (Dim, index)
import Direction (move, Direction (..))
import Data.Attributes as A
import Data.Attribute as F
import Data.Attribute (class Attr, Attribute)

import Data.Lens.Zoom (zoom)
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Typelevel.Num.Reps (D6)

newtype GameState = GameState
  { player :: EntityId
  , stamina :: Int
  , terrain :: Terrain
  , nextEntityId :: EntityId
  , rng :: Gen
  , entities :: Map EntityId EntityType
  , transformations :: Array Transformation
  , positions :: Bimap EntityId Position
  , hp :: Map EntityId Int
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
transform id entityType = execState $ do
  GameState { positions } <- get
  modify_ $ killEntity id
  createEntity $ EntityConfig
    { position: Bimap.lookup id positions
    , entityType
    }

tick :: GameState -> GameState
tick = tickItem <<< tickTransformations <<< checkSurvival

getEntitiesWithAttribute :: forall s a. Attr s a =>
  s -> GameState -> Array { entityId :: EntityId, attr :: a }
getEntitiesWithAttribute s (GameState {entities}) =
  flip foldMapWithIndex entities \entityId entityType ->
    case getAttribute s entityType of
         Nothing -> mempty
         Just attr -> [{entityId, attr}]

neighbors :: Position -> GameState -> Array EntityId
neighbors center gs = Array.catMaybes $
  (move <$> [U, D, L, R] <*> pure center) <#> \p -> getOccupant p gs

neighborRequirements :: Map Attribute (Array Attribute)
neighborRequirements = Map.fromFoldable
  [ Tuple F.root [F.rooting]
  , Tuple F.rooting [F.root]
  ]

checkSurvival :: GameState -> GameState
checkSurvival g@(GameState {positions}) =
  let dying = flip foldMapWithIndex positions \entityId position ->
              let et = getEntityType entityId g
                  reqs = flip foldMapWithIndex neighborRequirements
                    \is needs -> if hasFlag is et
                                 then needs
                                 else []
                  adj = flip getEntityType g <$> neighbors position g
                  satisfied req = any (hasFlag req) adj
               in if all satisfied reqs
                  then []
                  else [entityId]
   in flip execState g $ for_ dying \d -> modify_ $ killEntity d

tickTransformations :: GameState -> GameState
tickTransformations (GameState gs) =
  let {yes, no} = Array.partition ready (inc <$> gs.transformations)
      Endo f = foldMap Endo (apply <$> yes)
   in f $ GameState $ gs { transformations = no }
   where
   ready (Transformation { progress, duration }) = progress >= duration
   apply (Transformation { id, into }) = transform id into
   inc (Transformation t) = Transformation $ t{ progress = t.progress + 1 }

newtype EntityConfig = EntityConfig
  { position :: Maybe Position
  , entityType :: EntityType
  }

newGameState :: Gen -> GameState
newGameState rng =
  let playerId = EntityId 0
  in GameState
     { player: playerId
     , stamina: 6
     , positions: Bimap.singleton playerId (V{x:5,y:5})
     , terrain: initTerrain
     , entities:  Map.fromFoldable
         [ Tuple playerId Player
         ]
     , nextEntityId: EntityId 1
     , rng
     , transformations: mempty
     , hp: mempty
     }

createEntity :: EntityConfig -> State GameState EntityId
createEntity (EntityConfig ec) = do
  GameState { nextEntityId } <- get
  let hp = getAttribute (SProxy :: SProxy "health") ec.entityType
  modify_ $ \(GameState gs) -> GameState gs
      { nextEntityId = increment nextEntityId
      , positions = case ec.position of
                      Nothing -> gs.positions
                      Just p -> Bimap.insert nextEntityId p gs.positions
      , entities = Map.insert nextEntityId ec.entityType gs.entities
      , hp = case hp of
                  Nothing -> gs.hp
                  Just h -> Map.insert nextEntityId h gs.hp
      }
  when (hasAttribute A.rooting ec.entityType) do
     gs <- get
     for_ ec.position \center ->
       for_ (getAdjacentEmptySpaces center gs) \p ->
         createEntity (EntityConfig { position: Just p, entityType: Roots })
  pure nextEntityId

tickItem :: GameState -> GameState
tickItem gs =
  let items = _.entityId <$> getEntitiesWithAttribute A.item gs
      collectibles = Array.filter (isJust <<< flip getEntityPosition gs) items
   in if Array.null collectibles
      then execState spawnItem gs
      else gs


spawnItem :: State GameState Unit
spawnItem = do
  q <- toQuadrant <<< playerPosition <$> get
  quadrantShift <- hoist $ intRange 1 3
  x <- hoist $ intRange (-1) 1
  y <- hoist $ intRange (-1) 1
  let p = fromQuadrant (q + quadrantShift) + V {x,y}
  (Tuple {entityType} _) <- hoist $ element $ entitiesWithAttribute A.item
  void $ createEntity (EntityConfig { position: Just p, entityType })
  where
  toQuadrant :: Position -> Int
  toQuadrant (V {x,y}) = case Tuple (x > 2) (y > 2) of
    Tuple true true   -> 0
    Tuple false true  -> 1
    Tuple false false -> 2
    Tuple true false  -> 3
  fromQuadrant :: Int -> Position
  fromQuadrant 0 = V { x: 4, y: 4 }
  fromQuadrant 1 = V { x: 1, y: 4 }
  fromQuadrant 2 = V { x: 1, y: 1 }
  fromQuadrant 3 = V { x: 4, y: 1 }
  fromQuadrant n = fromQuadrant (n `mod` 4)

getAdjacentEmptySpaces :: Position -> GameState -> Array Position
getAdjacentEmptySpaces pos g@(GameState { positions, terrain }) =
  Array.filter isGood $ move <$> [U, D, L, R] <*> pure pos
  where
  isGood p = not (isJust $ getOccupant p g) && isFreeTerrain p
  isFreeTerrain p = case index terrain p of
                         Nothing -> false
                         Just t -> not (blocksMovement t)

doAttack :: EntityId -> GameState -> GameState
doAttack id g@(GameState gs) =
  let newHp = fromMaybe 0 (Map.lookup id gs.hp) - 1
   in if newHp <= 0
      then killEntity id g
      else GameState gs { hp = Map.insert id newHp gs.hp }

killEntity :: EntityId -> GameState -> GameState
killEntity id (GameState gs) =
  GameState $ gs
    { positions = Bimap.delete id gs.positions
    , entities = Map.delete id gs.entities
    , hp = Map.delete id gs.hp
    , transformations = Array.filter
        (\(Transformation{id: x}) -> x /= id) gs.transformations
    }

getPlayer :: GameState -> EntityId
getPlayer (GameState {player}) = player

getEntityPosition :: EntityId -> GameState -> Maybe Position
getEntityPosition eid (GameState {positions}) = Bimap.lookup eid positions

getOccupant :: Position -> GameState -> Maybe EntityId
getOccupant p (GameState {positions}) = Bimap.lookupR p positions

playerPosition :: GameState -> Position
playerPosition gs = unsafeFromJust $ getEntityPosition (getPlayer gs) gs

checkEntityAttribute
  :: forall s a. Attr s a
   => s -> EntityId -> GameState -> Boolean
checkEntityAttribute s eid = isJust <<< getEntityAttribute s eid

getEntityAttribute
  :: forall s a. Attr s a
   => s -> EntityId -> GameState -> Maybe a
getEntityAttribute s eid = getAttribute s <<< getEntityType eid

getEntityType :: EntityId -> GameState -> EntityType
getEntityType eid (GameState {entities}) = unsafeFromJust $ Map.lookup eid entities

placeEntity :: EntityId -> Position -> State GameState Unit
placeEntity eid pos = do
  GameState { positions } <- get
  occupant <- getOccupant pos <$> get
  GameState gs <- case occupant of
                       Nothing -> get
                       Just o -> modify $ killEntity o
  put $ GameState gs { positions = Bimap.insert eid pos gs.positions }


alterStamina :: Int -> GameState -> GameState
alterStamina s (GameState gs@{stamina}) = GameState gs { stamina = stamina + s }

class Hoist m where
  hoist :: forall a. m a -> State GameState a

instance hoistRandom :: Hoist Random where
  hoist r =  do
    GameState {rng} <- get
    let {result, nextGen} = runRandom r rng
    modify_ $ \(GameState gs) -> GameState $ gs {rng = nextGen}
    pure result

instance hoistTerrain :: Hoist (StateT (Dim D6 D6 Array TerrainType) Identity) where
  hoist = zoom $ _Newtype <<< prop (SProxy :: SProxy "terrain")

instance hoistPositions :: Hoist (StateT (Bimap EntityId (Vector Int)) Identity) where
  hoist = zoom $ _Newtype <<< prop (SProxy :: SProxy "positions")

instance hoistEntities :: Hoist (StateT (Map EntityId EntityType) Identity) where
  hoist = zoom $ _Newtype <<< prop (SProxy :: SProxy "entities")

instance hoistTransformations :: Hoist (StateT (Array Transformation) Identity) where
  hoist = zoom $ _Newtype <<< prop (SProxy :: SProxy "transformations")
