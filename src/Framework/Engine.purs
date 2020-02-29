module Framework.Engine where

import Extra.Prelude

import Framework.UI (UI (..), UIAwaitingInput)
import FRP.Event (create, subscribe, sampleOn, Event)
import Control.Monad.Rec.Class (tailRec, Step(..))

type EngineConfig gameState gameAction uiState input renderContext =
  { inputs :: Event input
  , ui :: UIAwaitingInput gameState gameAction uiState input
  , init :: gameState
  , step :: gameState -> gameAction -> Maybe gameState
  , ctx :: renderContext
  , draw :: renderContext -> uiState -> gameState -> Effect Unit
  }

runEngine
  :: forall gs a ui i ctx
   . EngineConfig gs a ui i ctx
  -> Effect (Effect Unit)
runEngine { inputs, ui, init, step, ctx, draw } = do
  { event: engineState, push: pushEngineState } <- create
  -- redraw screen in response to state changes
  cancelDraw <- subscribe engineState $
    \{uia: {uiRender, next}, gs} -> draw ctx uiRender gs
  -- step the game in response to user actions
  cancelEngine <-
    subscribe (sampleOn engineState (stepEngine step <$> inputs)) pushEngineState
  pushEngineState { uia: ui, gs: init }
  pure $ do
     cancelEngine
     cancelDraw

type EngineState gs a ui i =
  { uia :: UIAwaitingInput gs a ui i
  , gs :: gs }

stepEngine :: forall gs a ui i
  . (gs -> a -> Maybe gs)
  -> i
  -> EngineState gs a ui i
  -> EngineState gs a ui i
stepEngine step input { uia: {uiRender, next}, gs: g} = tailRec go { ui: (next input), gs: g }
  where
  go :: { ui :: UI gs a ui i, gs :: gs }
     -> Step { ui :: UI gs a ui i, gs :: gs } (EngineState gs a ui i)
  go { ui, gs } =
    case ui of
         UIAwaitingInput uia -> Done { uia, gs }
         UIAction { uiAction, next: cont } ->
           case step gs uiAction of
                Nothing -> Loop { ui: cont gs, gs }
                Just nextGs -> Loop { ui: cont nextGs, gs: nextGs }
