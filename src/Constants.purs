module Constants where

import Extra.Prelude

font :: String
font = "16px monospace"

charWidth :: Number
charWidth = 10.0

charHeight :: Number
charHeight = 16.0

tileMapDimensions :: { width ∷ Int, height ∷ Int, padding :: Int }
tileMapDimensions =
  { width: 16
  , height: 16
  , padding: 1
  }

displayDimensions
  :: { width ∷ Int, height ∷ Int, drawWidth :: Int, drawHeight :: Int }
displayDimensions = { width: 10, height: 10, drawWidth: 64, drawHeight: 64 }

canvasDimensions :: { width ∷ Number, height ∷ Number }
canvasDimensions = { width: 640.0, height: 640.0 }

newtype Color = Color String
derive instance newtypeColor :: Newtype Color _

black :: Color
black = Color "#000000"

white :: Color
white = Color "#FFFFFF"

gray :: Color
gray = Color "#C0C0C0"

blue :: Color
blue = Color "#0000FF"
