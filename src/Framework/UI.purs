module Framework.UI where

data UI gameState action uiData inputData =
    UIAwaitingInput (UIAwaitingInput gameState action uiData inputData)
  | UIAction (UIAction gameState action uiData inputData)

type UIAwaitingInput gs a ui id =
  { uiRender :: ui
  , next :: id -> UI gs a ui id
  }

type UIAction gs a ui id =
  { uiAction :: a
  , next :: gs -> UI gs a ui id
  }
