module XMonad.Config.Evaryont.Settings (
      layout_hook,
      log_hook,
      startup_hook,
      handle_events,
      event_hook,
      iconspaces,
      terminal_choice
      ) where

import System.Environment
import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.UpdateFocus
import XMonad.Actions.UpdatePointer
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Util.Cursor
import qualified XMonad.Hooks.EwmhDesktops as Ewmh

import XMonad.Config.Evaryont.Utils

terminal_choice :: String
terminal_choice = "urxvt"

log_hook = updatePointer (Relative 0.5 0.5)

startup_hook = adjustEventInput >> setDefaultCursor xC_left_ptr

handle_events = hintsEventHook
            <+> focusOnMouseMove
            <+> positionStoreEventHook

-- Handle various events, automatically. (Instead of the usual
-- wait-for-next-focus-event).
event_hook = docksEventHook
         <+> serverModeEventHook
         <+> fullscreenEventHook

--iconspaces :: String -> [WorkspaceId]
iconspaces = [ wrapBitmap "arch_10x10.xbm"
             , wrapBitmap "fox.xbm"
             , wrapBitmap "dish.xbm"
             , wrapBitmap "cat.xbm"
             , wrapBitmap "empty.xbm"
             , wrapBitmap "mail.xbm"
             , wrapBitmap "bug_02.xbm"
             , wrapBitmap "eye_l.xbm"
             , wrapBitmap "eye_r.xbm"
             ]
             where
                wrapBitmap bitmap = "^p(" ++ spacing ++ ")^i(" ++ bitmaps_path ++ bitmap ++ ")^p(" ++ spacing ++ ")"
                bitmaps_path      = "/home/colin/.icons/dzen" -- Location of dzen icon
                spacing           = "5" -- # of pixels padding on left & right


layout_hook = avoidStruts $ layoutHintsToCenter $ smartBorders $
              onWorkspace (iconspaces !! 1) simpleTabbed $
              layoutHook kde4Config

-- vim: set nospell:
