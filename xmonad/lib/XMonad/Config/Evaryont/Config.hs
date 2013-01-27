{-# LANGUAGE FlexibleContexts #-}
module XMonad.Config.Evaryont.Config (
      evaryontConfig
      ) where

import System.IO (Handle, hPutStrLn)

import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig

import XMonad.Config.Evaryont.Keys (key_bindings)
import XMonad.Config.Evaryont.Management (management_hook)
import XMonad.Config.Evaryont.Settings

-- XMonad's reason d'etierre
evaryontConfig = ewmh defaultConfig {
           terminal          = terminal_choice
         , modMask           = mod4Mask
         , manageHook        = management_hook
         , logHook           = log_hook
         , startupHook       = startup_hook
         , handleEventHook   = handle_events <+> fullscreenEventHook
         , layoutHook        = layout_hook
         , workspaces        = iconspaces
         , focusFollowsMouse = True
         } `additionalKeysP` key_bindings
