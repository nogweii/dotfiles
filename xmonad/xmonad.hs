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
-- Arch users: install haskell-xdg-basedir from AUR
import System.Environment.XDG.BaseDir
import System.IO

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
monoColorizer = colorRangeFromClassName
                     black  -- lowest inactive bg
                     black  -- highest inactive bg
                     black  -- active bg
                     white  -- inactive fg
                     white  -- active fg
  where black = minBound
        white = maxBound

pandoraGSConfig = (buildDefaultGSConfig monoColorizer) {
      gs_cellheight = 30
    , gs_cellwidth  = 100
    , gs_navigate   = pandoraNavigation
}

pianobarCmd :: Char -> IO ()
pianobarCmd x = do path <- getUserConfigFile "pianobar" "ctl"
                   writeFile path [x]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
key_bindings = [ ("M-x",             pandoraSelect) ]
--             , ()
--]
    where
        pandoraSelect = gridselect pandoraGSConfig [("||", io $ (pianobarCmd 'p')),
                                                    ("X8", io $ (pianobarCmd '-')),
                                                    (">>", io $ (pianobarCmd 'n')),
                                                    ("..", io $ (pianobarCmd 't')),
                                                    ("<3", io $ (pianobarCmd '+'))]
--                      >>= foldMap spawn
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
