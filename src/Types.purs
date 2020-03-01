module Types
  ( module Types
  , module Data.Attribute
  , module Data.Terrain
  , module Data.Sprite
  , module Data.Position
  )
  where

import Data.Terrain (Terrain, TerrainType (..), getTerrainSprite)
import Data.Attribute (Attribute (..))
import Data.Sprite (Sprite (..), spriteAt)
import Data.Position (Position)
import Direction (Direction)
import Framework.UI (UI, UIAwaitingInput, UIAction) as UIF

newtype GameState = GameState
  { player :: Position
  , terrain :: Terrain
  }

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
