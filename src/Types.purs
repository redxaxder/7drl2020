module Types
  ( module Types
  , module Data.Attribute
  , module Data.Terrain
  , module Data.Sprite
  , module Data.Position
  , module GameState
  , module Entity
  )
  where


import Data.Attribute (Attribute (..), blocking, attackable, root, plant)
import Data.Position (Position)
import Data.Sprite (Sprite (..), spriteAt)
import Data.Terrain (Terrain, TerrainType (..), getTerrainSprite)
import Direction (Direction)
import Framework.UI (UI, UIAwaitingInput, UIAction) as UIF
import GameState (GameState (..), playerPosition, getEntityType)
import Entity (EntityType(..), EntityId, hasAttribute)


type Key = String

data Action =
  StartGame
  | Move Direction
  | Attack EntityId

type UI = UIF.UI GameState Action UIState Key
type UIAwaitingInput = UIF.UIAwaitingInput GameState Action UIState Key
type UIAction = UIF.UIAction GameState Action UIState Key

data UIState
  = MainGame
  | StartScreen
  | GameOverScreen
  