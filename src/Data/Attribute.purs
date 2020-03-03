module Data.Attribute where

import Extra.Prelude

import Data.Variant (Variant, inj, prj, unvariant, Unvariant (..))
import Data.Symbol (SProxy (..), reflectSymbol, class IsSymbol)
import Data.Sprite (Sprite)
import Prim.Row (class Cons)

type AttributeType =
  ( blocking :: Unit
  , attackable :: Unit
  , plant :: Int
  , rooting :: Unit
  , root :: Unit
  , sprite :: Sprite
  , health :: Int
  )

isAttribute
  :: forall sym a r1
   . Cons sym a r1 AttributeType
  => IsSymbol sym
  => SProxy sym -> Attribute -> Boolean
isAttribute s x = case prjAttribute s x of
  Nothing -> false
  Just _ -> true

prjAttribute
  :: forall sym a r1
   . Cons sym a r1 AttributeType
  => IsSymbol sym
  => SProxy sym -> Attribute -> Maybe a
prjAttribute s (Attribute x) = prj s x

unsafePrjAttribute
  :: forall sym a r1
   . Cons sym a r1 AttributeType
  => IsSymbol sym
  => SProxy sym -> Attribute -> a
unsafePrjAttribute s = unsafeFromJust <<< prjAttribute s

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
rooting = Attribute $ inj (SProxy :: SProxy "rooting") unit

root :: Attribute
root = Attribute $ inj (SProxy :: SProxy "root") unit

blocking :: Attribute
blocking = Attribute $ inj (SProxy :: SProxy "blocking") unit

attackable :: Attribute
attackable = Attribute $ inj (SProxy :: SProxy "attackable") unit

-------------------------------------------------------------------------------
-- Attributes with data
-------------------------------------------------------------------------------

sprite :: Sprite -> Attribute
sprite = Attribute <<< inj (SProxy :: SProxy "sprite")
prjSprite :: Attribute -> Maybe Sprite
prjSprite (Attribute a) = prj (SProxy :: SProxy "sprite") a

health :: Int -> Attribute
health = Attribute <<< inj (SProxy :: SProxy "health")
prjHealth :: Attribute -> Maybe Int
prjHealth (Attribute a) = prj (SProxy :: SProxy "health") a

plant :: Int -> Attribute
plant = Attribute <<< inj (SProxy :: SProxy "plant")
prjPlant :: Attribute -> Maybe Int
prjPlant (Attribute a) = prj (SProxy :: SProxy "plant") a
