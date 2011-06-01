module XMonad.Config.Evaryont.Keys (
      key_bindings
      ) where

import XMonad
import XMonad.Config.Evaryont.Utils
import Music.Pandora
import XMonad.Actions.CycleWS

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
key_bindings = [ ("M-a",           pandoraSelect) -- control pianobar
               , ("M-<Escape>",    kill) -- close current window
               , ("M-S-<Escape>",  spawn "xkill") -- click to kill an application
               , ("M-q",           spawn "gnome-screensaver-command -l") -- lock the screen
               , ("M-S-q",         broadcastMessage ReleaseResources >> restart "xmonad" True) -- restart XMonad
               , ("M-C-q",         spawn "gnome-session-quit") -- shutdown/hibernate/suspend dialog
               , ("M-l",           moveTo Next NonEmptyWS) -- go to next workspace
               , ("M-h",           moveTo Prev NonEmptyWS) -- go to previous workspace
               , ("M-S-l",         moveTo Next EmptyWS) -- create a new, empty, workspace
               , ("M-S-h",         moveTo Prev EmptyWS)
               , ("M-<Backspace>", toggleWS) -- toggle which workspace you're on
               , ("M-t",           toggleFloat) -- toggle wether a window is floating
               , ("M-e",           nextScreen) -- go to the next physical screen (Xinerama)
               ]