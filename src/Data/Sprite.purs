module Data.Sprite where

import Extra.Prelude

newtype Sprite = Sprite { offsetX :: Int, offsetY :: Int }

derive instance eqSprite :: Eq Sprite
derive instance ordSprite :: Ord Sprite

spriteAt :: Int -> Int -> Sprite
spriteAt offsetX offsetY = Sprite { offsetX, offsetY }

stamina :: Sprite
stamina = spriteAt 26 11

blank :: Sprite
blank = spriteAt 0 0

