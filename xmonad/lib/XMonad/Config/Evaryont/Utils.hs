module XMonad.Config.Evaryont.Utils (
      toggleFloat,
      dbusCommand,
      kdeOverride,
    --fromMaybe,
    --printWindowTitle,
      captureHook,
      capturePredicate,
      ) where

import XMonad
import XMonad.Core (WindowSpace, WorkspaceId)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Util.WindowProperties (getProp32s)

import Control.Monad.Trans
import System.FilePath
import System.Directory

-- Will toggle floating status on the currently focused window. Hand made, this
-- one I wrote all on my own!
toggleFloat = withFocused $ \windowId -> do
  floats <- gets (W.floating . windowset)
  if windowId `M.member` floats
    then withFocused $ windows . W.sink
    else float windowId

-- Uses qdbus to execute a DBus method. Infeffecitve, but it works. Just builds
-- the string itself, to be executed elsewhere (thus, not involving IO())
dbusCommand :: String -> String -> String -> String
dbusCommand dest interface method = "qdbus " ++ dest ++ " " ++
                                    interface ++ " " ++ method

-- Returns whether or not the window is set using KDE's window type atom
kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
    override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
    wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
    return $ maybe False (elem $ fromIntegral override) wt

-- A predicate used by WorkspaceCapture, will return true when the given value
-- is not the named scratchpads workspace.
capturePredicate x = return $ x `notElem` ["NSP"]

-- A hook used by WorkspaceCapture, moves the generated screenshot file to a
-- different directory.
captureHook scrotPath =
  do homeDirectory <- getHomeDirectory
     renameFile scrotPath (homeDirectory </> "media/pictures/xmonad" </> scrotPath)
