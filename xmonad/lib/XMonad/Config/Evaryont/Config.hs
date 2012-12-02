{-# LANGUAGE FlexibleContexts #-}
module XMonad.Config.Evaryont.Config (
      evaryontConfig
      ) where

import System.IO (Handle, hPutStrLn)

import XMonad
import XMonad.Config.Kde
import XMonad.Util.EZConfig

import XMonad.Config.Evaryont.Keys (key_bindings)
import XMonad.Config.Evaryont.Management (management_hook)
import XMonad.Config.Evaryont.Settings
import XMonad.Config.Evaryont.Statusbar

-- XMonad's reason d'etierre
--evaryontConfig :: (LayoutClass l Window, Read (l Window)) => Handle -> XConfig l
--evaryontConfig workspace_pipe = kde4Config {
evaryontConfig workspace_pipe = defaultConfig {
           terminal        = terminal_choice
         , modMask         = mod4Mask
         , manageHook      = management_hook
         , logHook         = (workspace_bar workspace_pipe) <+> log_hook
         , startupHook     = startup_hook
         , handleEventHook = handle_events
         , layoutHook      = layout_hook
         , workspaces      = iconspaces
         } `additionalKeysP` key_bindings
