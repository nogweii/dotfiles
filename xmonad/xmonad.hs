import XMonad
import XMonad.Config.Gnome

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- for dbus (gnome-session hang)
import System.Environment
import System.Cmd
import Control.Concurrent

import XMonad.Actions.GridSelect
import XMonad.Util.EZConfig

import Data.Foldable

controlPandora :: (Integral a) => a -> String
controlPandora 1 = controlPandora 0 ++ "PlayPause"
controlPandora 2 = controlPandora 0 ++ "LoveCurrentSong"
controlPandora 3 = controlPandora 0 ++ "BanCurrentSong"
controlPandora 4 = controlPandora 0 ++ "SkipSong"
controlPandora 5 = controlPandora 0 ++ "TiredCurrentSong"
controlPandora x = "qdbus net.kevinmehall.Pithos /net/kevinmehall/Pithos"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
key_bindings = [ ("M-x",             pandoraSelect) ]
--             , ()
--]
    where
        pandoraSelect = gridselect defaultGSConfig [("||", controlPandora 1),
                                                    ("X8", controlPandora 3),
                                                    (">>", controlPandora 4),
                                                    ("..", controlPandora 5),
                                                    ("<3", controlPandora 2)]
                        >>= foldMap spawn
--compiled_bindings = \c -> mkKeymap c $ key_bindings

-- main = xmonad gnomeConfig {
--        modMask     = mod4Mask,
--        terminal    = "urxvt"
-- }
main = xmonad $ gnomeConfig {
       terminal           = "urxvt"
     , borderWidth        = 2
     , normalBorderColor  = "black"
     , focusedBorderColor = "orange"
     , focusFollowsMouse  = True
     , modMask            = mod4Mask
     , keys               = keys gnomeConfig
--   , mouseBindings      = myMouseBindings
--   , layoutHook         = myLayout
     , handleEventHook    = ewmhDesktopsEventHook
     , startupHook        = ewmhDesktopsStartup
--   , logHook            = myLogHook
--   , manageHook         = myManageHook
    } `additionalKeysP` key_bindings

--- vim: set syn=haskell nospell:
