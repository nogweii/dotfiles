module XMonad.Config.Evaryont.Statusbar (
      status_bar,
      workspace_bar,
      workspace_dzen_command
      ) where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspace)
import System.IO (Handle, hPutStrLn)
import Data.List

import XMonad.Config.Evaryont.Settings (iconspaces)

status_bar :: String
status_bar = "dzen2"

terminus_font :: String
terminus_font = "-*-terminus-*-*-*-*-12-*-*-*-*-*-iso8859-*"

workspace_dzen_command :: String
workspace_dzen_command = "dzen2 -x '0' -y '0' -h '16' -w '956' -ta 'l' -fg 'gray80' -bg 'black' -fn '"++terminus_font++"' -p -e ''"

workspace_bar :: Handle -> X ()
workspace_bar h = dynamicLogWithPP $ defaultPP
    { ppOutput          = hPutStrLn h
    , ppSort            = fmap (namedScratchpadFilterOutWorkspace.) (ppSort defaultPP) -- hide "NSP" from workspace list
    , ppOrder           = orderText
    , ppExtras          = []
    , ppSep             = "^p(5)" -- "^fg(" ++ "gray20" ++ ")|"
    , ppWsSep           = "^p(-2)"
    , ppCurrent         = dzenColor "blue"    "black"
    , ppUrgent          = dzenColor "green"   "black" . wrapClickWorkSpace . (\a -> (a,a))
    , ppVisible         = dzenColor "gray20"  "black" . wrapClickWorkSpace . (\a -> (a,a))
    , ppHidden          = dzenColor "gray80"  "black" . wrapClickWorkSpace . (\a -> (a,a))
    , ppHiddenNoWindows = dzenColor "gray20"  "black" . wrapClickWorkSpace . (\a -> (a,a))
    , ppLayout          = dzenColor "blue"    "black" . wrapClickLayout -- . "lol"
    , ppTitle           = dzenColor "gray80"  "black" . wrapClickTitle . titleText . dzenEscape
    }
    where
        orderText (ws:l:t:_)         = [ws,l,t]
        titleText []                 = dzenColor "gray20" "black" "(desktop)"
        titleText x                  = shorten 82 x
        --wrapClickLayout content      = "^ca(1,xdotool key super+space)" ++ content ++ "^ca()"  -- clickable layout
        wrapClickLayout content      = "^ca(1,xdotool key super+space)lol^ca()"  -- clickable layout
        wrapClickTitle content       = "^ca(1,xdotool key super+j)" ++ content ++ "^ca()"      -- clickable title
        -- clickable workspaces
        wrapClickWorkSpace (idx,str) = "^ca(1,xdotool key super+" ++ index ++ ")" ++ "^ca(3,xdotool key super+shift+" ++ index ++ ")" ++ str ++ "^ca()^ca()"
          where
              wsIdxToString Nothing = "1"
              wsIdxToString (Just n) = show (n+1)
              index = wsIdxToString (elemIndex idx iconspaces)
