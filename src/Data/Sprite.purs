module Data.Sprite where

import Extra.Prelude

newtype Sprite = Sprite { offsetX :: Int, offsetY :: Int }

derive instance eqSprite :: Eq Sprite
derive instance ordSprite :: Ord Sprite

spriteAt :: Int -> Int -> Sprite
spriteAt offsetX offsetY = Sprite { offsetX, offsetY }

