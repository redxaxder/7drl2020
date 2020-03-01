module Types
  ( module Types
  , module Data.Attribute
  , module Data.Tile
  , module Data.Sprite
  , module Data.Position
  )
  where

import Data.Attribute (Attribute (..))
import Data.Tile (Tile (..))
import Data.Sprite (Sprite (..), spriteAt)
import Data.Position (Position)
import Direction (Direction)
import Framework.UI (UI, UIAwaitingInput, UIAction) as UIF

import DimArray (Dim)
import Data.Typelevel.Num.Reps (D8)

newtype GameState = GameState
  { player :: Position
  , terrain :: Terrain
  }

type Terrain = Dim D8 D8 Array TerrainType
data TerrainType = Dirt

getTerrainSprite :: TerrainType -> Sprite
getTerrainSprite Dirt = spriteAt 2 0

type Key = String

data Action =
  StartGame
  | Move Direction

type UI = UIF.UI GameState Action UIState Key
type UIAwaitingInput = UIF.UIAwaitingInput GameState Action UIState Key
type UIAction = UIF.UIAction GameState Action UIState Key

data UIState
  = MainGame
  | StartScreen
