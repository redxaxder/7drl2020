module Data.Sprite where

import Extra.Prelude

newtype Sprite = Sprite { offsetX :: Int, offsetY :: Int }

derive instance eqSprite :: Eq Sprite
derive instance ordSprite :: Ord Sprite

spriteAt :: Int -> Int -> Sprite
spriteAt offsetX offsetY = Sprite { offsetX, offsetY }

stamina :: Sprite
stamina = spriteAt 26 11

attackUp :: Sprite
attackUp = spriteAt 30 11

noTrip :: Sprite
noTrip = spriteAt 7 23

timeFreeze :: Sprite
timeFreeze = spriteAt 26 24

onlyGrass :: Sprite
onlyGrass = spriteAt 14 25

blank :: Sprite
blank = spriteAt 0 0

one :: Sprite
one = spriteAt 20 29
two :: Sprite
two = spriteAt 21 29
three :: Sprite
three = spriteAt 22 29

