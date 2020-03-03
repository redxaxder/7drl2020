module Data.Attribute where

import Extra.Prelude

import Data.Variant (Variant, inj, prj, unvariant, Unvariant (..))
import Data.Symbol (SProxy (..), reflectSymbol, class IsSymbol)
import Data.Sprite (Sprite)

type AttributeType =
  ( blocking :: Unit
  , attackable :: Unit
  , plant :: Unit
  , rooting :: Unit
  , root :: Unit
  , sprite :: Sprite
  , health :: Int
  )

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

plant :: Attribute
plant = Attribute $ inj (SProxy :: SProxy "plant") unit

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
