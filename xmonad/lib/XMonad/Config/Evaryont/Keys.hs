module XMonad.Config.Evaryont.Keys (
      key_bindings,
      toggleStrutsKey
      ) where

import System.Environment
import XMonad
import XMonad.Config.Evaryont.Utils
import XMonad.Config.Evaryont.Logout
import Music.Pandora
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Config.Evaryont.Settings
import XMonad.Config.Evaryont.Utils (captureHook, capturePredicate)
import XMonad.Util.WorkspaceScreenshot

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
key_bindings = [ ("M-a",           pandoraSelect) -- control pianobar
               , ("M-<Escape>",    kill) -- close current window
               , ("M-S-<Escape>",  spawn "xkill") -- click to kill an application
               , ("M-q",           spawn lockScreen) -- lock the screen
            -- , ("M-S-q",         broadcastMessage ReleaseResources >> restart "xmonad" True) -- restart XMonad
               , ("M-S-q",         restart "xmonad" True) -- restart XMonad
               , ("M-C-q",         logoutDialog) -- shutdown/hibernate/suspend dialog
               , ("M-l",           moveTo Next NonEmptyWS) -- go to next workspace
               , ("M-h",           moveTo Prev NonEmptyWS) -- go to previous workspace
               , ("M-S-l",         moveTo Next EmptyWS) -- create a new, empty, workspace
               , ("M-S-h",         moveTo Prev EmptyWS)
               , ("M-<Backspace>", toggleWS) -- toggle which workspace you're on
               , ("M-t",           toggleFloat) -- toggle wether a window is floating
               , ("M-e",           nextScreen) -- go to the next physical screen (Xinerama)
               , ("M-p",           spawn "dmenu-run") -- application launcher
               , ("M-S-p",         spawn "gnome-panel-screenshot -i") -- take a screenshot
               , ("M-x",           goToSelected defaultGSConfig) -- application launcher
               , ("M-<Return>",    spawn terminal_choice) -- Launch the default terminal
               , ("M-<Print>",     captureWorkspacesWhen defaultPredicate captureHook horizontally) -- Screenshot all the workspaces
               , ("M-?",           spawn "show-xmonad-keys") -- List all keys configured in xmonad
               , ("M-S-?",         spawn "show-xmonad-keys") -- List all keys configured in xmonad
               ]

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- vim: set nospell:
