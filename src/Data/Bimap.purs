module Data.Bimap where

import Extra.Prelude

import Data.Map as Map


data Bimap a b = Bimap (Map a b) (Map b a)

lookup :: forall a b. Ord a => a -> Bimap a b -> Maybe b
lookup x (Bimap l _)  = Map.lookup x l

lookupR :: forall a b. Ord b => b -> Bimap a b -> Maybe a
lookupR x (Bimap _ r)  = Map.lookup x r

insert :: forall a b. Ord a => Ord b => a -> b -> Bimap a b -> Bimap a b
insert x y = delete x >>> deleteR y >>> unsafeInsert x y

delete :: forall a b. Ord a => a -> Bimap a b -> Bimap a b
delete x (Bimap l r) = Bimap (Map.delete x l) r

deleteR :: forall a b. Ord b => b -> Bimap a b -> Bimap a b
deleteR x (Bimap l r) = Bimap l (Map.delete x r)

unsafeInsert :: forall a b. Ord a => Ord b => a -> b -> Bimap a b -> Bimap a b
unsafeInsert x y (Bimap l r) = Bimap (Map.insert x y l) (Map.insert y x r)

singleton :: forall a b. a -> b -> Bimap a b
singleton a b = Bimap (Map.singleton a b) (Map.singleton b a)

leftMap :: forall a b. Bimap a b -> Map a b
leftMap (Bimap l _) = l

rightMap :: forall a b. Bimap a b -> Map b a
rightMap (Bimap _ r) = r
