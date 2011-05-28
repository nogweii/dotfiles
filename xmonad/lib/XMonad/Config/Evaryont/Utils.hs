module XMonad.Config.Evaryont.Utils (
      toggleFloat
      ) where

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

toggleFloat = withFocused $ \windowId -> do
  floats <- gets (W.floating . windowset)
  if windowId `M.member` floats
    then withFocused $ windows . W.sink
    else float windowId
