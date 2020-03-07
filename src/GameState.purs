module GameState where

import Extra.Prelude

import Data.Bimap (Bimap)
import Data.Array as Array
import Data.Bimap as Bimap
import Data.Map as Map
import Data.Position (Position)
import Data.Terrain (Terrain, initTerrain, TerrainType, blocksMovement)
import Entity (EntityType (..), EntityId (..), increment, hasAttribute, getAttribute, hasFlag, entitiesWithAttribute)
import Random (Gen, Random, runRandom, element, unsafeElement)
import DimArray (Dim, index)
import Direction (move, Direction (..))
import Data.Attributes as A
import Data.Attribute as R
import Data.Attribute (class Attr, Attribute)

import Data.Lens.Zoom (zoom)
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Typelevel.Num.Reps (D6)
import Data.Unfoldable as Unfoldable

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
  , score :: Int
  , inventory :: Array EntityId
  , playerDidBurn :: Boolean
  , attackBuff :: Int
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
  killEntity id
  createEntity $ EntityConfig
    { position: Bimap.lookup id positions
    , entityType
    }

tick :: GameState -> GameState
tick = clearBurn <<< tickItem <<< execState doCellularLogic <<< tickTransformations

clearBurn :: GameState -> GameState
clearBurn (GameState gs) = GameState gs { playerDidBurn = false }

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
  [ Tuple R.root [R.rooting]
  , Tuple R.rooting [R.root]
  ]


doCellularLogic :: State GameState Unit
doCellularLogic = do
  -- snapshot of the gamestate as it was when we started thid
  -- we want to use this (rather than the current state) for all checks
  -- all modification happens to the State variable, but all querying happens
  -- using g
  g@(GameState {positions,playerDidBurn}) <- get
  -- first, check "starvation" of plants that dont have required neighbors
  let starving = flip foldMapWithIndex positions \entityId position ->
              let et = unsafeGetEntityType entityId g
                  reqs = flip foldMapWithIndex neighborRequirements
                    \is needs -> if hasFlag is et
                                 then needs
                                 else []
                  adj = flip unsafeGetEntityType g <$> neighbors position g
                  satisfied req = any (hasFlag req) adj
               in if all satisfied reqs
                  then []
                  else [entityId]
  for_ starving killEntity
  -- kill overcrowded plants
  -- TODO
  -- next, do vine damage
  traverse_ (modify_ <<< doAttack) $
    flip foldMapWithIndex positions \entityId position ->
      case checkEntityAttribute A.parasitic entityId g of
        false -> mempty
        true -> Array.filter
          (\neighborId -> checkEntityAttribute A.parasiteTarget neighborId g)
          (neighbors position g)
  -- next, do fire effects
  let doFireEffect :: Array EntityId -> {entityId :: EntityId, fireEffect :: R.Consequence } -> State GameState (Array EntityId)
      doFireEffect safe { entityId, fireEffect: R.Death } = do
        killEntity entityId
        pure safe
      doFireEffect safe { entityId, fireEffect: R.Burn } = do
        modify_ $ doAttack entityId
        pure safe
      doFireEffect safe { entityId, fireEffect: R.Dry } = do
        modify_ $ \(GameState gs) ->
          GameState gs { entities = Map.insert entityId DryGrass gs.entities }
        pure safe
      doFireEffect safe { entityId, fireEffect: R.Flash } = do
        case getEntityPosition entityId g of
             Nothing -> pure safe
             Just p -> if isNothing $ Array.elemIndex entityId safe
               then do
                 killEntity entityId
                 burnNeighbors (Array.cons entityId safe) p
               else pure safe
      burnNeighbors :: Array EntityId -> Position -> State GameState (Array EntityId)
      burnNeighbors safe position =
        let go acc entityId =
              case getEntityAttribute A.burns entityId g of
                Nothing -> pure acc
                Just fireEffect -> doFireEffect acc { entityId, fireEffect }
         in foldM go safe (neighbors position g)
      playerBurn = if playerDidBurn
        then Array.singleton $ playerPosition g
        else mempty
      fires = flip foldMapWithIndex positions \entityId position ->
                      if checkEntityAttribute A.flame entityId g
                        then Array.singleton {entityId, position}
                        else mempty
      burnPositions = playerBurn <> (_.position <$> fires)
  _ <- foldM burnNeighbors mempty burnPositions
  for_ (_.entityId <$> fires) (modify_ <<< doAttack)

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
     , score: 0
     , inventory: mempty
     , attackBuff: 0
     , playerDidBurn: false
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
  playerQuadrant <- toQuadrant <<< playerPosition <$> get
  GameState {terrain} <- get
  let spawnOptions = do
        x <- Array.range 0 5
        y <- Array.range 0 5
        let p = V{x,y}
        guard $ toQuadrant p /= playerQuadrant
        guard $ fromMaybe false $ not <<< blocksMovement <$> index terrain p
        pure p
  p <- hoist $ unsafeElement spawnOptions
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

doPlayerAttack :: EntityId -> GameState -> GameState
doPlayerAttack id g = flip execState g do
  (GameState gs@{attackBuff}) <- get
  let attack = if gs.attackBuff > 0 then 3 else 1
  put $ GameState gs { attackBuff = max 0 $ attackBuff - 1 }
  (modifyEntityHp id (\x -> x-attack))

doAttack :: EntityId -> GameState -> GameState
doAttack id g = execState (modifyEntityHp id (\x -> x-1)) g

modifyEntityHp :: EntityId -> (Int -> Int) -> State GameState Unit
modifyEntityHp eid f = do
  (GameState gs@{hp}) <- get
  let newHp = f $ fromMaybe 0 (Map.lookup eid gs.hp)
  if newHp <= 0
    then killEntity eid
    else put $ GameState gs { hp = Map.insert eid newHp hp }

collectItem :: EntityId -> State GameState Unit
collectItem id = do
  (GameState g@{score,inventory,positions}) <- get
  put $ GameState g { score = score + 1
                    , inventory = Array.snoc (Array.takeEnd 2 inventory) id
                    , positions = Bimap.delete id positions
                    }

consumeItem :: Int -> State GameState Unit
consumeItem i = do
  g@(GameState gs) <- get
  let meid = Array.index gs.inventory i
  let itemEffect = do
        eid <- meid
        getEntityAttribute A.item eid g
  let inventory = fromMaybe gs.inventory $ Array.deleteAt i gs.inventory
  let ets = case meid of
        Nothing -> gs.entities
        Just eid -> Map.delete eid gs.entities
  put $ GameState gs { inventory = inventory, entities = ets }
  case itemEffect of
    Just R.Restore -> modify_ $ alterStamina 2
    Just R.Fire -> modify_ $ \(GameState x) -> GameState x { playerDidBurn = true }
    Just R.AttackUp -> modify_ $ \(GameState x) -> GameState x { attackBuff = 4 }
    _ -> pure unit

killEntity :: EntityId -> State GameState Unit
killEntity id = do
  g@(GameState {playerDidBurn} ) <- get
  let mp = getEntityPosition id g
      doScatter = checkEntityAttribute A.scatter id g
      doFire = fromMaybe false $ do
         fireResponse <- getEntityAttribute A.burns id g
         _ <- Array.elemIndex fireResponse [R.Burn, R.Flash]
         pure true
      doVine = checkEntityAttribute A.parasiteTarget id g
      neighbor = fromMaybe ff $ mp <#> \pos ->
         flip any (neighbors pos g) \n ->
           { doFire: checkEntityAttribute A.flame n g
                 || (n == (getPlayer g) && playerDidBurn )
           , doVine: checkEntityAttribute A.parasitic n g
           }
      spawnHere = {doFire,doVine} && neighbor
  when doScatter $ do
    let spawnLocations = do
          p <- Unfoldable.fromMaybe mp
          getAdjacentEmptySpaces p g
    for_ spawnLocations spawnPlant
  case spawnHere of
       { doFire: true } -> do
          let fireHp = fromMaybe 1 $ getEntityAttribute A.health id g
          eid <- createEntity (EntityConfig { entityType: Fire, position: mp })
          modifyEntityHp eid (\_ -> fireHp)
       { doVine: true } -> 
         void $ createEntity (EntityConfig { entityType: Vine, position: mp })
       _ -> pure unit

  modify_ $ \(GameState gs) -> GameState gs
    { positions = Bimap.delete id gs.positions
    , entities = Map.delete id gs.entities
    , hp = Map.delete id gs.hp
    , transformations = Array.filter
        (\(Transformation{id: x}) -> x /= id) gs.transformations
    }

spawnPlant :: Position -> State GameState Unit
spawnPlant p =  do
  (Tuple {entityType} duration) <- hoist $ element (entitiesWithAttribute A.plant)
  id <- createEntity (EntityConfig { position: Just p, entityType: Seed })
  hoist $ addTransformation $ Transformation
    { id
    , into: entityType
    , progress: -1
    , duration
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
getEntityAttribute s eid gs = getAttribute s =<< getEntityType eid gs

getEntityType :: EntityId -> GameState -> Maybe EntityType
getEntityType eid (GameState {entities}) = Map.lookup eid entities

unsafeGetEntityType :: EntityId -> GameState -> EntityType
unsafeGetEntityType eid (GameState {entities}) = unsafeFromJust $ Map.lookup eid entities

placeEntity :: EntityId -> Position -> State GameState Unit
placeEntity eid pos = do
  GameState { positions } <- get
  occupant <- getOccupant pos <$> get
  case occupant of
    Nothing -> pure unit
    Just o -> killEntity o
  modify_ $ \(GameState gs) ->
    GameState gs { positions = Bimap.insert eid pos gs.positions }

alterStamina :: Int -> GameState -> GameState
alterStamina s (GameState gs@{stamina}) =
  GameState gs { stamina = clamp $ stamina + s }
  where
  clamp x = max (min x 6) 0

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
