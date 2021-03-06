module DimArray where

import Extra.Prelude

import Data.Array as Array
import Data.FunctorWithIndex(class FunctorWithIndex, mapWithIndex)

import Data.Typelevel.Num.Sets (class Nat, toInt')
import Type.Proxy (Proxy(..))

newtype Dim w h f a = Dim (f a)

class Indexable (f :: Type -> Type) where
  ix :: forall a. f a -> Int -> Maybe a
  ins :: forall a. Int -> a -> f a -> Maybe (f a)

instance indexableArray :: Indexable Array where
  ix = Array.index
  ins = Array.updateAt

index :: forall w h a f. Indexable f => Nat w => Nat h
  => Dim w h f a -> Vector Int -> Maybe a
index (Dim array) i =
  toLinearIndex (Proxy :: Proxy w) (Proxy :: Proxy h) i >>= ix array

updateAt :: forall w h a f. Indexable f => Nat w => Nat h
  => Vector Int -> a -> Dim w h f a -> Maybe (Dim w h f a)
updateAt v x (Dim array) = do
  i <- toLinearIndex (Proxy :: Proxy w) (Proxy :: Proxy h) v
  Dim <$> ins i x array

toLinearIndex :: forall w h. Nat w => Nat h => Proxy w -> Proxy h -> Vector Int -> Maybe Int
toLinearIndex w h (V {x,y}) = do
  let w' = toInt' w
      h' = toInt' h
  guard $ x >= 0
  guard $ y >= 0
  guard $ x < w'
  guard $ y < h'
  pure $ y * w' + x

fromLinearIndex :: forall w. Nat w => Proxy w -> Int -> Vector Int
fromLinearIndex w i =
  let width = toInt' w
   in V {x: i `mod` width, y: i `div` width }

instance functorDimArray :: Functor f => Functor (Dim w h f) where
  map f (Dim arr) = Dim (map f arr)

instance functorWithIndexDimArray
  :: (FunctorWithIndex Int f, Nat w)
  => FunctorWithIndex (Vector Int) (Dim w h f)  where
  mapWithIndex f (Dim arr) =
    let f' i = f (fromLinearIndex (Proxy :: Proxy w) i)
     in Dim (mapWithIndex f' arr)

instance foldableDimArray :: Foldable f => Foldable (Dim w h f) where
  foldr f z (Dim a) = foldr f z a
  foldl f z (Dim a) = foldl f z a
  foldMap f (Dim a) = foldMap f a

instance foldableWithIndexDimArray
  :: (FoldableWithIndex Int f, Nat w)
  => FoldableWithIndex (Vector Int) (Dim w h f) where
  foldrWithIndex f z (Dim a) =
    let f' i = f (fromLinearIndex (Proxy :: Proxy w) i)
     in foldrWithIndex f' z a
  foldlWithIndex f z (Dim a) =
    let f' i = f (fromLinearIndex (Proxy :: Proxy w) i)
     in foldlWithIndex f' z a
  foldMapWithIndex f (Dim a) =
    let f' i = f (fromLinearIndex (Proxy :: Proxy w) i)
     in foldMapWithIndex f' a

instance traversableDimArray :: Traversable f => Traversable (Dim w h f) where
  traverse f (Dim arr) = Dim <$> traverse f arr
  sequence (Dim arr) = Dim <$> sequence arr

instance traversableWithIndexDimArray
  :: (TraversableWithIndex Int f, Nat w)
  => TraversableWithIndex (Vector Int) (Dim w h f) where
  traverseWithIndex f (Dim a) =
    let f' i = f (fromLinearIndex (Proxy :: Proxy w) i)
     in Dim <$> traverseWithIndex f' a
