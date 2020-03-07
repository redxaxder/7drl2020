module Data.Sprite where

import Extra.Prelude
import Data.Char as Char

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

digitSprite :: Int -> Sprite
digitSprite i  = spriteAt (19 + i) 29

letterSprite :: Char -> Sprite
letterSprite c =
  if      c >= 'a' && c <= 'm' then spriteAt (19 + shift c 'a') 30
  else if c >= 'A' && c <= 'M' then spriteAt (19 + shift c 'A') 30
  else if c >= 'n' && c <= 'z' then spriteAt (19 + shift c 'n') 31
  else if c >= 'N' && c <= 'Z' then spriteAt (19 + shift c 'N') 31
  else blank
  where shift a b = Char.toCharCode a - Char.toCharCode b

