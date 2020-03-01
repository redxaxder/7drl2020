module Data.Sprite where

newtype Sprite = Sprite { offsetX :: Int, offsetY :: Int }

spriteAt :: Int -> Int -> Sprite
spriteAt offsetX offsetY = Sprite { offsetX, offsetY }

player :: Sprite
player = spriteAt 26 7

dirt :: Sprite
dirt = spriteAt 2 0
