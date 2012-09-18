module XMonad.Config.Evaryont.Settings (
      settings
      ) where

import XMonad
import XMonad.Actions.UpdateFocus
import XMonad.Actions.UpdatePointer
import XMonad.Config.Evaryont.Utils
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.EZConfig
import XMonad.Util.WindowProperties (getProp32s)
import qualified XMonad.Hooks.EwmhDesktops as Ewmh

manage_hook = composeAll [
       isKDETrayWindow               --> doIgnore
     , isFullscreen                  --> doFullFloat
     , resource  =? "gxmessage"      --> doCenterFloat
     , resource  =? "xmessage"       --> doCenterFloat
     , resource  =? "kdesktop"       --> doIgnore
     , className =? "plasma-desktop" --> doIgnore
     , className =? "Plasma"         --> doIgnore
     , className =? "Klipper"        --> doFloat
       ]
     <+> manageDocks
     <+> transience'
     <+> (kdeOverride --> doFloat)

layout_hook = avoidStruts $ layoutHintsToCenter $ smartBorders $ layoutHook kde4Config

log_hook = takeTopFocus >> updatePointer (Relative 0.5 0.5)

startup_hook = adjustEventInput
handle_events = hintsEventHook <+> focusOnMouseMove

-- apps such as chrome emit correct ewmh events and are handled properly
-- while apps such as vlc use other fullscreen event messages and require
-- X.L.Fullscreen, hence the use of E.fullscreenEventHook and the
-- XMonad.Layout.fullscreenEventHook below
event_hook = Ewmh.ewmhDesktopsEventHook
         <+> Ewmh.fullscreenEventHook
         <+> fullscreenEventHook

-- PLC from Google issues noticed the inconsistencies with XMonad and the
-- default kde4Config's manage hook. Here is the reported & improved version as
-- reported by plc. Use it as a basis for any good, solid KDE config
plcplcConfig = kde4Config {
             manageHook  = ((className =? "krunner") >>= return . not --> manageHook kde4Config)
                       <+> (kdeOverride --> doFloat)
           }

-- XMonad's reason d'etierre
settings = kde4Config {
           terminal        = "konsole"
         , modMask         = mod4Mask
         , manageHook      = manage_hook   <+> manageHook      kde4Config
         , logHook         = log_hook      <+> logHook         kde4Config
         , startupHook     = startup_hook  <+> startupHook     kde4Config
         , handleEventHook = handle_events <+> handleEventHook kde4Config
         , layoutHook      = layout_hook
         }

-- vim: set nospell:
