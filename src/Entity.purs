module Entity where

import Extra.Prelude

import Data.Sprite (Sprite, spriteAt)
import Data.Map as Map
import Data.Set as Set
import Data.Attribute
  ( Attribute
  , attrName
  , sprite
  , prjSprite
  , health
  , prjHealth
  , blocking
  , attackable
  , rooting
  , root
  , plant
  )

import Extra.Prelude

newtype EntityId = EntityId Int

derive instance eqEntityId :: Eq EntityId
derive instance ordEntityId :: Ord EntityId

data EntityType = Grass | Player | Tree | Seed | Roots

data SpawnEffect = MkRoots
data Need = NeedRoots

derive instance eqEntityType :: Eq EntityType
derive instance ordEntityType :: Ord EntityType

type EntityRow =
  { entityType :: EntityType
  , attributes :: Set Attribute
  }

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
  , t Seed   [ spriteAttr 1 0 ]
  , t Grass  [ spriteAttr 5 0, plant ]
  , t Tree   [ spriteAttr 0 1, health 3, plant, rooting, blocking, attackable]
  , t Roots  [ spriteAttr 16 1, root ]
  ]

increment :: EntityId -> EntityId
increment (EntityId eid) = EntityId (eid + 1)

hasAttribute :: Attribute -> EntityType -> Boolean
hasAttribute a et =  Set.member a (lookupEntity et).attributes

healthAttribute :: EntityType -> Maybe Int
healthAttribute et = prjHealth =<< find (\x -> attrName x == "health")
  (lookupEntity et).attributes

spriteAttribute :: EntityType -> Maybe Sprite
spriteAttribute et = prjSprite =<< find (\x -> attrName x == "sprite")
  (lookupEntity et).attributes
