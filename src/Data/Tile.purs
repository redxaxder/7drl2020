module Data.Tile where

import Extra.Prelude

import Data.Sprite (Sprite, spriteAt)

data Tile = Floor | Wall

derive instance eqTile :: Eq Tile

blocksVision :: Tile -> Boolean
blocksVision Wall = true
blocksVision _ = false

blocksMovement :: Tile -> Boolean
blocksMovement Wall = true
blocksMovement _ = false

tileSprite :: Tile -> Sprite
tileSprite Floor = spriteAt 2 0
tileSprite Wall = spriteAt 3 0
