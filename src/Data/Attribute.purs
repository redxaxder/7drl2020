module Data.Attribute where

import Extra.Prelude

import Data.Variant (Variant, inj, prj, unvariant, Unvariant (..))
import Data.Sprite (Sprite)
import Prim.Row (class Cons)
import Data.Attributes as A

type AttributeType =
  ( blocking :: Unit
  , attackable :: Unit
  , plant :: { growth :: Int, difficulty :: Int }
  , rooting :: Unit
  , root :: Unit
  , sprite :: Sprite
  , health :: Int
  , impedes :: Int
  , item :: ItemEffect
  , scatter :: Unit
  , parasitic :: Unit
  , parasiteTarget :: Unit
  , burns :: Consequence
  , flame :: Unit
  , crowded :: Consequence
  )

data ItemEffect = Restore | Fire | AttackUp | NoTrip | TimeFreeze | OnlyGrass
data Consequence = Flash | Burn | Dry | Death

derive instance eqItemEffect :: Eq ItemEffect
derive instance ordItemEffect :: Ord ItemEffect
derive instance eqBurn :: Eq Consequence
derive instance ordBurn :: Ord Consequence

class Attr s a | s -> a where
  prjAttribute :: s -> Attribute -> Maybe a

instance attrSym
  :: (Cons sym a r1 AttributeType, IsSymbol sym)
  => Attr (SProxy sym) a
  where
  prjAttribute s (Attribute x) = prj s x

isAttribute
  :: forall s a . Attr s a => s -> Attribute -> Boolean
isAttribute s x = case prjAttribute s x of
  Nothing -> false
  Just _ -> true

unsafePrjAttribute
  :: forall s a. Attr s a => s -> Attribute -> a
unsafePrjAttribute s = unsafeFromJust <<< prjAttribute s

withAttribute :: forall b. Attribute -> (forall s a. Attr s a => s -> a -> b) -> b
withAttribute (Attribute x) k = let Unvariant f = unvariant x in f k

newtype Attribute = Attribute (Variant AttributeType)

derive instance eqAttribute :: Eq Attribute
derive instance ordAttribute :: Ord Attribute

attrName :: Attribute -> String
attrName (Attribute x) =
  let Unvariant f = unvariant x
   in f getTag
   where
   getTag :: forall s x. IsSymbol s => SProxy s -> x -> String
   getTag tag _ = reflectSymbol tag
-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

rooting :: Attribute
rooting = Attribute $ inj A.rooting unit

root :: Attribute
root = Attribute $ inj A.root unit

blocking :: Attribute
blocking = Attribute $ inj A.blocking unit

attackable :: Attribute
attackable = Attribute $ inj A.attackable unit

scatter :: Attribute
scatter = Attribute $ inj A.scatter unit

parasitic :: Attribute
parasitic = Attribute $ inj A.parasitic unit

parasiteTarget :: Attribute
parasiteTarget = Attribute $ inj A.parasiteTarget unit

flame :: Attribute
flame = Attribute $ inj A.flame unit

-------------------------------------------------------------------------------
-- Attributes with data
-------------------------------------------------------------------------------

sprite :: Sprite -> Attribute
sprite = Attribute <<< inj A.sprite

health :: Int -> Attribute
health = Attribute <<< inj A.health

-- growth time
plant :: Int -> Int -> Attribute
plant growth difficulty = Attribute <<< inj A.plant $ { growth, difficulty }

-- stamina cost
impedes :: Int -> Attribute
impedes = Attribute <<< inj A.impedes

item :: ItemEffect -> Attribute
item = Attribute <<< inj A.item

burns :: Consequence -> Attribute
burns = Attribute <<< inj A.burns

crowded :: Consequence -> Attribute
crowded = Attribute <<< inj A.crowded
