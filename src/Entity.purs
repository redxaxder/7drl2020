module Entity where

import Extra.Prelude
import Data.Sprite (Sprite, spriteAt)
import Data.Map as Map

newtype EntityId = EntityId Int

derive instance eqEntityId :: Eq EntityId
derive instance ordEntityId :: Ord EntityId

data EntityType = Grass | Player | Tree

derive instance eqEntityType :: Eq EntityType
derive instance ordEntityType :: Ord EntityType

entitySprite :: EntityType -> Sprite
entitySprite et = (unsafeFromJust $ Map.lookup et entityTable).sprite

type EntityRow =
  { sprite :: Sprite
  }

lookupEntity :: EntityType -> EntityRow
lookupEntity et = unsafeFromJust $ Map.lookup et entityTable

t :: EntityType -> Sprite -> Tuple EntityType EntityRow
t et sprite = Tuple et { sprite }

entityTable :: Map EntityType EntityRow
entityTable = Map.fromFoldable
  [ t Player (spriteAt 26 7)
  , t Grass  (spriteAt 5 0)
  , t Tree   (spriteAt 0 1)
  ]

increment :: EntityId -> EntityId
increment (EntityId eid) = EntityId (eid + 1)
