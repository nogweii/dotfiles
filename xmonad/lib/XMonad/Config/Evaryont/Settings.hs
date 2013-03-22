module XMonad.Config.Evaryont.Settings (
      log_hook,
      startup_hook,
      handle_events,
      iconspaces,
      terminal_choice
      ) where

import System.Environment
import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.UpdateFocus
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Util.Cursor
import XMonad.Layout.Fullscreen (fullscreenEventHook)
import XMonad.Layout.LayoutHints (hintsEventHook)
import qualified XMonad.Hooks.EwmhDesktops as Ewmh

import Data.Maybe (fromMaybe)
import XMonad.Util.WindowProperties (getProp32)
import qualified XMonad.StackSet as W

import XMonad.Config.Evaryont.Utils

terminal_choice :: String
terminal_choice = "urxvt"

log_hook = updatePointer (Relative 0.5 0.5)
        >> fadeInactiveLogHook 0xbbbbbbbb
        >> Ewmh.ewmhDesktopsLogHook
           -- Abuse function needs to come after ewmh, so that it will overwrite
           -- the _NET_CLIENT_LIST atom
        >> abuseClientList

startup_hook = adjustEventInput
            >> setDefaultCursor xC_left_ptr
            >> Ewmh.ewmhDesktopsStartup

-- Handle various events, automatically. (Instead of the usual
-- wait-for-next-focus to parse any changed events).
handle_events = hintsEventHook
            <+> focusOnMouseMove
            <+> positionStoreEventHook
            <+> serverModeEventHook
            <+> fullscreenEventHook
            <+> Ewmh.fullscreenEventHook
            <+> Ewmh.ewmhDesktopsEventHook
            <+> docksEventHook

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


-- This is an utter abuse of the CLIENT_LIST atom, but meh. I want my panels to
-- only show the currently focused window, any nothing else. However, the only
-- source the panels will respond to is this list. So, every time the focus
-- changes, update the list to contain *only* the currently active window
abuseClientList :: X ()
abuseClientList = withWindowSet $ \s -> withDisplay $ \dpy -> do
    let w = fromMaybe none (W.peek s)
    r <- asks theRoot
    c <- getAtom "WINDOW"
    a <- getAtom "_NET_CLIENT_LIST"
    io $ changeProperty32 dpy r a c propModeReplace [fromIntegral w]
    a' <- getAtom "_NET_CLIENT_LIST_STACKING"
    io $ changeProperty32 dpy r a' c propModeReplace [fromIntegral w]


-- vim: set nospell:
