module Graphics.Render where

import Extra.Prelude

import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Data.String.CodePoints as String
import Effect.Aff (Aff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Data.Array as Array

import Constants (tileMapDimensions, canvasDimensions, font, black, Color(..), displayDimensions, charWidth, charHeight)
import Graphics.Canvas as Canvas
import Types (Sprite (..))

newtype Context = Context { context :: Canvas.Context2D, spritesheet :: Canvas.CanvasImageSource }

initCanvas :: { canvasId :: String, spritesheetPath :: String } -> Aff (Maybe Context)
initCanvas { canvasId, spritesheetPath } = runMaybeT do
  canvas <- MaybeT $ liftEffect $ Canvas.getCanvasElementById canvasId
  liftEffect $ Canvas.setCanvasDimensions canvas canvasDimensions
  context <- liftEffect $ Canvas.getContext2D canvas
  liftEffect $ setImageSmoothing context false
  liftEffect $ Canvas.setFont context font
  spritesheet <- lift $ makeAff \handler -> do
    Canvas.tryLoadImage spritesheetPath (handler <<< maybe (Left $ error "failed to load image") pure)
    mempty
  pure $ Context { context, spritesheet }

foreign import setImageSmoothing :: Canvas.Context2D -> Boolean -> Effect Unit

drawSprite :: Context -> Sprite -> Vector Number -> Effect Unit
drawSprite (Context {context, spritesheet}) (Sprite { offsetX, offsetY }) (V { x, y }) =
  let
    { width, height, padding } = tileMapDimensions
    { drawWidth, drawHeight} = displayDimensions
    sourceX = toNumber (offsetX * (width + padding))
    sourceY = toNumber (offsetY * (height + padding))
    w = toNumber width
    h = toNumber height
    p = toNumber padding
  in
  Canvas.drawImageFull context spritesheet sourceX sourceY w h x y (toNumber drawWidth) (toNumber drawHeight)

drawSpriteToGrid :: Context -> Sprite -> Vector Int -> Effect Unit
drawSpriteToGrid ctx sprite (V { x, y }) =
  let
    { drawWidth, drawHeight } = displayDimensions
    canvasX = toNumber (x * drawWidth)
    canvasY = toNumber (y * drawHeight)
  in
  when
    ( 0 <= x && x < displayDimensions.width
      && 0 <= y && y < displayDimensions.height
      ) $ drawSprite ctx sprite (V {x: canvasX, y: canvasY})
    -- (Canvas.drawImageFull context spritesheet sourceX sourceY w h canvasX canvasY w h)

drawDotsToGrid :: Context -> Int -> Int -> Vector Int -> Effect Unit
drawDotsToGrid (Context {context, spritesheet}) ndots totalDots (V { x, y } ) =
  let
    sourceX = toNumber 517
    sourceY = toNumber 504
    size = 2
    { drawWidth, drawHeight } = displayDimensions
    baseX = x * drawWidth
    baseY = toNumber $ y * drawHeight
    width = drawWidth / totalDots - totalDots
    height = toNumber $ drawHeight / 8
    f i = Canvas.drawImageFull context spritesheet sourceX sourceY (toNumber size) (toNumber size) (finalX i) baseY (toNumber width) height
    finalX i = toNumber $ baseX + (i - 1) * size * width
   in
    traverse_ f (Array.range 1 ndots)

getTextDimensions :: String -> { width :: Number, height :: Number }
getTextDimensions t = { width: charWidth * (toNumber $ String.length t), height: charHeight }

drawText :: Context -> Color -> Number -> Number -> String -> Effect Unit
drawText ctx@(Context {context}) color x y text = do
  let {width, height} = getTextDimensions text
  clearRegion ctx {x,y, width, height}
  setFillStyle ctx color
  Canvas.fillText context text (textOffset.x + x) (textOffset.y + y)

drawTextToGrid :: Context -> Color -> String -> Vector Int -> Effect Unit
drawTextToGrid ctx color text (V {x,y}) = drawText ctx color x' y' text
  where
   x' = toNumber x * charWidth
   y' = toNumber y * charHeight

drawLinesToGrid :: Context -> Color -> Vector Int -> Array String -> Effect Unit
drawLinesToGrid ctx color (V {x,y}) texts = forWithIndex_  texts \i t ->
  drawTextToGrid ctx color t (V {x, y: y + i})


clear :: Context -> Effect Unit
clear ctx@(Context{context}) = do
  setFillStyle ctx black
  Canvas.fillRect context { x: 0.0, y: 0.0, width: canvasDimensions.width, height: canvasDimensions.height }

clearRegion :: Context -> {x :: Number, y :: Number, width :: Number, height :: Number} -> Effect Unit
clearRegion ctx@(Context {context}) rect = do
  setFillStyle ctx black
  Canvas.fillRect context rect

setFillStyle :: Context -> Color -> Effect Unit
setFillStyle (Context {context}) (Color c) = Canvas.setFillStyle context c

textOffset :: { x ∷ Number, y ∷ Number }
textOffset = { x: 3.0, y: 13.0 }
