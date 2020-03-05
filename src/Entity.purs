module Entity where

import Extra.Prelude

import Data.Sprite (spriteAt)
import Data.Map as Map
import Data.Set as Set
import Data.Attribute
  ( Attribute
  , class Attr
  , sprite
  , prjAttribute
  , health
  , blocking
  , attackable
  , rooting
  , root
  , plant
  , isAttribute
  , unsafePrjAttribute
  , withAttribute
  , impedes
  , item
  )

import Data.Array as Array
import Data.Array.NonEmpty as NArray

newtype EntityId = EntityId Int

derive instance eqEntityId :: Eq EntityId
derive instance ordEntityId :: Ord EntityId

data EntityType = Grass | Player | Tree | Seed | Roots | Apple | Pear | Carrot

data SpawnEffect = MkRoots
data Need = NeedRoots

derive instance eqEntityType :: Eq EntityType
derive instance ordEntityType :: Ord EntityType

type EntityRow =
  { entityType :: EntityType
  , attributes :: Set Attribute
  }

entitiesWithAttribute
   :: forall s a. Attr s a
   => s -> NonEmptyArray (Tuple EntityRow a)
entitiesWithAttribute s = unsafeFromJust
  $ NArray.fromArray
  $ Array.catMaybes
  $ map getThing
  $ Array.fromFoldable entityTable
  where
  getThing x =
    Tuple x <$> unsafePrjAttribute s <$> find (isAttribute s) x.attributes

lookupEntity :: EntityType -> EntityRow
lookupEntity et = unsafeFromJust $ Map.lookup et entityTable

t :: EntityType
  -> Array Attribute
  -> Tuple EntityType EntityRow
t entityType attrs =
  Tuple entityType { entityType, attributes: Set.fromFoldable attrs }

spriteAttr :: Int -> Int -> Attribute
spriteAttr a b = sprite (spriteAt a b)

entityTable :: Map EntityType EntityRow
entityTable = Map.fromFoldable
  [ t Player [ spriteAttr 26 7 ]
  , t Seed   [ spriteAttr 13 0 ]
  , t Grass  [ spriteAttr 0 2, plant 1 ]
  , t Tree   [ spriteAttr 0 1, health 3, plant 6, rooting, blocking, attackable]
  , t Roots  [ spriteAttr 16 1, root, impedes 1 ]
  , t Apple  [ spriteAttr 15 29, item ]
  , t Pear   [ spriteAttr 16 29, item ]
  , t Carrot [ spriteAttr 18 30, item ]
  ]

increment :: EntityId -> EntityId
increment (EntityId eid) = EntityId (eid + 1)

hasFlag :: Attribute -> EntityType -> Boolean
hasFlag a et = withAttribute a k
  where
  k :: forall s a. Attr s a => s -> a -> Boolean
  k s _ = hasAttribute s et

hasAttribute :: forall s a. Attr s a => s -> EntityType -> Boolean
hasAttribute s et =  isJust $ getAttribute s et

getAttribute :: forall s a. Attr s a => s -> EntityType -> Maybe a
getAttribute s et = prjAttribute s =<< find
  (isAttribute s) (lookupEntity et).attributes
