import XMonad
import XMonad.Config.Gnome

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Util.EZConfig

-- from ~/.xmonad/lib/
import Music.Pandora

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
key_bindings = [ ("M-x",             pandoraSelect) ]

main = xmonad $ gnomeConfig {
       terminal           = "urxvt"
     , borderWidth        = 2
     , focusFollowsMouse  = True
     , modMask            = mod4Mask
     , keys               = keys gnomeConfig
     , handleEventHook    = ewmhDesktopsEventHook
     , startupHook        = ewmhDesktopsStartup
    } `additionalKeysP` key_bindings

--- vim: set syn=haskell nospell:
