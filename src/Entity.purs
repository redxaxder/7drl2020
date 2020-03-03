module Entity where

import Extra.Prelude
import Data.Sprite (Sprite, spriteAt)
import Data.Map as Map

newtype EntityId = EntityId Int

derive instance eqEntityId :: Eq EntityId
derive instance ordEntityId :: Ord EntityId

data EntityType = Grass | Player | Tree | Seed

derive instance eqEntityType :: Eq EntityType
derive instance ordEntityType :: Ord EntityType

type EntityRow =
  { sprite :: Sprite
  , blocking :: Boolean
  , attackable :: Boolean
  , hp :: Maybe Int
  }

lookupEntity :: EntityType -> EntityRow
lookupEntity et = unsafeFromJust $ Map.lookup et entityTable

t :: EntityType -> Sprite -> Boolean -> Boolean -> Maybe Int -> Tuple EntityType EntityRow
t et sprite blocking attackable hp = Tuple et { sprite, blocking, attackable, hp }

entityTable :: Map EntityType EntityRow
entityTable = Map.fromFoldable
  --  Type    Sprite         Blocking Attackable Hp
  [ t Player (spriteAt 26 7) true     false      Nothing
  , t Grass  (spriteAt 5 0)  false    false      Nothing
  , t Tree   (spriteAt 0 1)  true     true       (Just 3)
  , t Seed   (spriteAt 1 0)  false    false      Nothing
  ]

increment :: EntityId -> EntityId
increment (EntityId eid) = EntityId (eid + 1)
