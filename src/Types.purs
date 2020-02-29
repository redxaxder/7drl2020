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
import Data.Sprite (Sprite (..))
import Data.Position (Position)
import Direction (Direction)
import Framework.UI (UI, UIAwaitingInput, UIAction) as UIF

type GameState = { player :: Position }

type Key = String

data Action = Move Direction

type UI = UIF.UI GameState Action UIState Key
type UIAwaitingInput = UIF.UIAwaitingInput GameState Action UIState Key
type UIAction = UIF.UIAction GameState Action UIState Key

data UIState
  = MainGame
  | StartScreen
