import XMonad
import XMonad.Config.Kde
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutHints (layoutHintsToCenter)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Hooks.DynamicLog
import XMonad.Util.Loggers
import XMonad.Util.Font (Align (AlignCenter))
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.Replace

-- from ~/.xmonad/lib/
import Music.Pandora
import XMonad.Config.Evaryont.Keys

manage_hook = composeAll [
       isKDETrayWindow --> doIgnore
     , isFullscreen --> doFullFloat
     , resource =? "stalonetray" --> doIgnore
     , resource =? "gxmessage" --> doCenterFloat
     , resource =? "xmessage" --> doCenterFloat
     , className =? "Plasma" --> doFloat
     -- redirect a window to some workspace.
-- , className =? "Firefox"  --> doF (W.shift (myWorkspaces !! 1)))
       ]
     <+> manageDocks
     <+> manageHook kde4Config
     <+> transience'

layout_hook = smartBorders $ layoutHintsToCenter $ avoidStruts $ layoutHook kde4Config

-- pretty much every piece of the config is in lib/XMonad/Config/Evaryont - go
-- check that out instead of this boring main function!
settings = kde4Config {
           terminal   = "urxvt"
         , modMask    = mod4Mask
         , manageHook = manage_hook <+> manageHook kde4Config
         , layoutHook = layout_hook
         } `additionalKeysP` key_bindings

main :: IO ()
main = do
--     replace
--     h <- spawnPipe "startkde"
       xmonad settings

--- vim: set syn=haskell nospell:
