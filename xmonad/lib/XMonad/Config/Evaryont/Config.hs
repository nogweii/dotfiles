{-# LANGUAGE FlexibleContexts #-}
module XMonad.Config.Evaryont.Config (
      evaryontConfig
      ) where

import System.IO (Handle, hPutStrLn, hClose)
import qualified System.IO.UTF8

import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.UrgencyHook

import XMonad.Config.Evaryont.Keys (key_bindings)
import XMonad.Config.Evaryont.Management (management_hook)
import XMonad.Config.Evaryont.Settings
import XMonad.Config.Evaryont.Layout

--main = xmonad $ addDescrKeys ((mod4Mask, xK_F1), xMessage) myKeys
--                   defaultConfig { modMask = mod4Mask }
--
--myKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
--   [("M-x a", addName "useless message" $ spawn "xmessage foo"),
--    ("M-c", sendMessage' Expand)]
--    ^++^
--   [("<XF86AudioPlay>", spawn "mpc toggle" :: X ()),
--    ("<XF86AudioNext>", spawn "mpc next")]


evaryontConfig = withUrgencyHook NoUrgencyHook
--             $ withNavigation2DConfig myNavigation2DConfig
               $ addDescrKeys ((mod4Mask, xK_F1), showKeybindings) key_bindings configOne

configOne = defaultConfig
    { terminal          = terminal_choice
    , modMask           = mod4Mask
    , manageHook        = management_hook
    , logHook           = log_hook
    , handleEventHook   = handle_events <+> fullscreenEventHook
    , layoutHook        = layout_hook
    , workspaces        = iconspaces
    , focusFollowsMouse = True
    , startupHook       = startup_hook
    }

-- | Display keyboard mappings using zenity
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe "zenity --text-info"
  System.IO.UTF8.hPutStr h (unlines $ showKm x)
  hClose h
  return () 
