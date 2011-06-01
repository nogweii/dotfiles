import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutHints (layoutHintsToCenter)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Hooks.DynamicLog
import XMonad.Util.Loggers
import XMonad.Util.Font (Align (AlignCenter))
import XMonad.Util.Run (spawnPipe, hPutStrLn)

-- from ~/.xmonad/lib/
import Music.Pandora
import XMonad.Config.Evaryont.Keys

manage_hook = composeAll [
       isKDETrayWindow --> doIgnore
     , isFullscreen --> doFullFloat
     , resource =? "stalonetray" --> doIgnore
     , resource =? "gxmessage" --> doCenterFloat
     , resource =? "xmessage" --> doCenterFloat
     -- redirect a window to some workspace.
-- , className =? "Firefox"  --> doF (W.shift (myWorkspaces !! 1)))
       ]
     <+> manageDocks
     <+> manageHook gnomeConfig
     <+> transience'

layout_hook = smartBorders $ layoutHintsToCenter $ avoidStruts $ layoutHook gnomeConfig

dzen_pp :: PP
dzen_pp = defaultPP {  ppCurrent  = dzenColor "white" "#2b4f98" . pad
                     , ppVisible  = dzenColor "black" "#999999" . pad
                     , ppHidden   = dzenColor "black" "#cccccc" . pad
                     , ppHiddenNoWindows = const ""
                     , ppUrgent   = dzenColor "red" "yellow" . dzenStrip
                     , ppWsSep    = ""
                     , ppSep      = ""
                     , ppLayout   = dzenColor "black" "#cccccc" .
                                    (\ x -> case x of
                                              "TilePrime Horizontal" -> " TTT "
                                              "TilePrime Vertical"   -> " []= "
                                              "Hinted Full"          -> " [ ] "
                                              _                      -> pad x
                                    )
                     , ppTitle    = ("^bg(#324c80) " ++) . dzenEscape
                     }

customPP = defaultPP { ppCurrent = dzenColor "#429942" "" . wrap "<" ">"
                     , ppHidden = dzenColor "#C98F0A" ""
                     , ppHiddenNoWindows = const ""
                     , ppUrgent = dzenColor "#FFFFAF" "" . wrap "[" "]"
                     , ppLayout = dzenColor "#C9A34E" ""
                     , ppTitle = dzenColor "#C9A34E" "" . shorten 80
                     , ppSep = dzenColor "#429942" "" " | "
     , ppExtras = [wrapL "[" "]" $ date "%a %d %b %H.%M"]
     --, ppExtras = [lTitle, logSp 3, wrapL "[" "]" $ date "%a %d %b"]
--     , ppOrder = \(ws,l,_,xs) -> [l,ws] ++ xs
     }
   where
     lTitle = fixedWidthL AlignCenter " " 99 <$> dzenColorL "cornsilk3" "" <$>
              padL . shortenL 80 $ logTitle


dzen_windows conf = statusBar ("dzen2 " ++ flags) customPP toggleStrutsKey conf
 where -- n.b quoting in the following lines
    colors  = "-fg '#a8a3f7' -bg '#3f3c6d'"
    font    = " -fn 'xft:Liberation Mono:size=8'" -- n.b. leading space
    flags   = "-e 'onstart=lower' -h 32 -w 1920 -ta l "

myWorkspaces            :: [String]
myWorkspaces            = clickable . (map dzenEscape) $ ["1","2","3","4","5"]

  where clickable l     = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = i ]
-- pretty much every piece of the config is in lib/XMonad/Config/Evaryont - go
-- check that out instead of this boring main function!
settings = gnomeConfig {
           terminal   = "urxvt"
         , modMask    = mod4Mask
         , manageHook = manage_hook <+> manageHook gnomeConfig
         , layoutHook = layout_hook
         } `additionalKeysP` key_bindings

main :: IO ()
main = xmonad =<< dzen_windows settings

--- vim: set syn=haskell nospell:
