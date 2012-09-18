module XMonad.Config.Evaryont.Utils (
      toggleFloat,
      dbusCommand,
      kdeOverride
      ) where

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

toggleFloat = withFocused $ \windowId -> do
  floats <- gets (W.floating . windowset)
  if windowId `M.member` floats
    then withFocused $ windows . W.sink
    else float windowId

dbusCommand :: String -> String -> String -> String
dbusCommand dest interface method = "qdbus " ++ dest ++ " " ++
                                    interface ++ " " ++ method

kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
    override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
    wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
    return $ maybe False (elem $ fromIntegral override) wt

