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
import Data.Attribute as R

import Data.Array as Array
import Data.Array.NonEmpty as NArray

newtype EntityId = EntityId Int

derive instance eqEntityId :: Eq EntityId
derive instance ordEntityId :: Ord EntityId

data EntityType = Player | Seed | Roots | DryGrass | Fire
  | Grass | Tree | Pod | Vine
  | Apple | Pear | Carrot | Meat | Bread | Torch
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
  [ t Player   [ spriteAttr 26 7 ]
  -- spawnable plants
  , t Grass    [ spriteAttr 0 2, plant 1 0, R.burns R.Dry ]
  , t Tree     [ spriteAttr 0 1, health 3, plant 4 4, rooting, blocking, attackable, R.burns R.Burn, R.parasiteTarget]
  , t Pod    [ spriteAttr 20 5, plant 1 2, R.scatter, R.burns R.Death ]
  , t Vine   [ spriteAttr 2 2, plant 4 1, R.parasitic, attackable, blocking, R.burns R.Burn ]

  -- Consumables
  , t Meat   [ spriteAttr 16 28, item R.AttackUp ]
  , t Bread  [ spriteAttr 15 28, item R.NoTrip ]
  , t Apple  [ spriteAttr 15 29, item R.Restore ]
  -- , t Pear   [ spriteAttr 16 29, item R.OnlyGrass ]
  -- , t Carrot [ spriteAttr 18 30, item R.TimeFreeze ]
  , t Torch  [ spriteAttr 11 25, item R.Fire ]

  -- misc
  , t Seed     [ spriteAttr 13 0, R.burns R.Death ]
  , t Roots    [ spriteAttr 16 1, root, impedes 1 ]
  , t Fire     [ spriteAttr 15 10, blocking, R.flame ]
  , t DryGrass [ spriteAttr 21 2,  R.burns R.Flash ]
  ]

increment :: EntityId -> EntityId
increment (EntityId eid) = EntityId (eid + 1)

hasFlag :: Attribute -> EntityType -> Boolean
hasFlag a et = withAttribute a k
  where
  k :: forall s a. Attr s a => s -> a -> Boolean
  k s _ = hasAttribute s et

hasAttribute :: forall s a. Attr s a => s -> EntityType -> Boolean
hasAttribute s et = isJust $ getAttribute s et

getAttribute :: forall s a. Attr s a => s -> EntityType -> Maybe a
getAttribute s et = prjAttribute s =<< find
  (isAttribute s) (lookupEntity et).attributes
