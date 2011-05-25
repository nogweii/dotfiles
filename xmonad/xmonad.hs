import XMonad
import XMonad.Config.Gnome

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- for dbus (gnome-session hang)
import System.Environment
import System.Cmd
import Control.Concurrent

import XMonad.Util.EZConfig

-- I'm being silly & dangerous... fun!
--import XMonad.Actions.GridSelect
import XMonad.Darcs.GridSelect

import Data.Foldable
import qualified Data.Map as M

controlPandora :: (Integral a) => a -> String
controlPandora 1 = controlPandora 0 ++ ""
controlPandora 2 = controlPandora 0 ++ "LoveCurrentSong"
controlPandora 3 = controlPandora 0 ++ "BanCurrentSong"
controlPandora 4 = controlPandora 0 ++ "SkipSong"
controlPandora 5 = controlPandora 0 ++ "TiredCurrentSong"
controlPandora x = "qdbus net.kevinmehall.Pithos /net/kevinmehall/Pithos"

xdg_config :: String
xdg_config = fmap ("/" ++) (getEnv "XDG_CONFIG_HOME")

pianobarCommand :: Char -> String
pianobarCommand x = "echo '" ++ [x] ++ "' >" ++ xdg_config ++ "/pianobar/ctl"

pandoraNavigation :: TwoD a (Maybe a)
pandoraNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select >> pandoraNavigation) -- default: play/pause
         ,((0,xK_h)     , move (-1,0)   >> select >> pandoraNavigation) -- left: mark tired
         ,((0,xK_l)     , move (1,0)    >> select >> pandoraNavigation) -- right: skip
         ,((0,xK_j)     , move (0,1)    >> select >> pandoraNavigation) -- down: ban
         ,((0,xK_k)     , move (0,-1)   >> select >> pandoraNavigation) -- up: love
         ,((0,xK_space) , setPos (0,0)  >> select >> pandoraNavigation) -- center: play/pause
         ]
       -- The navigation handler ignores unknown key symbols
       navDefaultHandler = const pandoraNavigation

-- A green monochrome colorizer based on window class
greenColorizer = colorRangeFromClassName
                     black            -- lowest inactive bg
                     (0x70,0xFF,0x70) -- highest inactive bg
                     black            -- active bg
                     white            -- inactive fg
                     white            -- active fg
  where black = minBound
        white = maxBound

gsconfig2 pandoraGSConfig = (buildDefaultGSConfig greenColorizer) {
      gs_cellheight = 30
    , gs_cellwidth  = 100
    , gs_navigate   = pandoraNavigation
}

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
key_bindings = [ ("M-x",             pandoraSelect) ]
--             , ()
--]
    where
        pandoraSelect = gridselect defaultGSConfig [("||", controlPandora 1),
                                                    ("X8", controlPandora 3),
                                                    (">>", controlPandora 4),
                                                    ("..", controlPandora 5),
                                                    ("<3", controlPandora 2)]
                        >>= foldMap spawn
--compiled_bindings = \c -> mkKeymap c $ key_bindings

-- main = xmonad gnomeConfig {
--        modMask     = mod4Mask,
--        terminal    = "urxvt"
-- }
main = xmonad $ gnomeConfig {
       terminal           = "urxvt"
     , borderWidth        = 2
     , normalBorderColor  = "black"
     , focusedBorderColor = "orange"
     , focusFollowsMouse  = True
     , modMask            = mod4Mask
     , keys               = keys gnomeConfig
--   , mouseBindings      = myMouseBindings
--   , layoutHook         = myLayout
     , handleEventHook    = ewmhDesktopsEventHook
     , startupHook        = ewmhDesktopsStartup
--   , logHook            = myLogHook
--   , manageHook         = myManageHook
    } `additionalKeysP` key_bindings

--- vim: set syn=haskell nospell:
