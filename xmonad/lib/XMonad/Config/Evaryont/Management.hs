module XMonad.Config.Evaryont.Management (
      management_hook,
      ) where

import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.PositionStoreHooks
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W

import XMonad.Config.Evaryont.Utils
import XMonad.Config.Evaryont.Settings (iconspaces)
import Data.List

-- Scratchpad
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect 0 (1/50) 1 (3/4))
scratchPad = scratchpadSpawnActionCustom "urxvtc -name scratchpad"

-- Manage hook
management_hook :: ManageHook
management_hook = (composeAll . concat $
  [ [resource   =? roles   --> doIgnore                     | roles   <- ignore_window_types     ]
  , [className  =? classes --> doIgnore                     | classes <- ignoramous              ]
  , [className  =? classes --> doShift (iconspaces !! 1)    | classes <- web_shift               ]
  , [className  =? classes --> doShift (iconspaces !! 4)    | classes <- chat_shift              ]
  , [className  =? classes --> doShift (iconspaces !! 3)    | classes <- gimp_shift              ]
  , [className  =? classes --> doF W.focusDown              | classes <- avoid_focus             ]
  , [className  =? classes --> doCenterFloat                | classes <- center_floaters_by_class]
  , [name       =? names   --> doCenterFloat                | names   <- center_floaters_by_name ]
  , [name       =? names   --> doSideFloat NW               | names   <- move_to_side            ]
  , [isFullscreen          --> doFullFloat                                                       ]
  , [isKDETrayWindow       --> doIgnore                                                          ]
  -- If I launch gvim from Luakit, it'll have the title "blah.txt - LUAKIT",
  -- center only those gvims.
  , [className  =? "Gvim" <&&> fmap ("LUAKIT" `isSuffixOf`) title --> doCenterFloat]
  ]) <+> manageScratchPad
     <+> manageDocks
     <+> transience'
     <+> positionStoreManageHook Nothing
     <+> (kdeOverride --> doFloat)
    where
    doShiftAndGo ws          = doF (W.greedyView ws) <+> doShift ws
    role                     = stringProperty "WM_WINDOW_ROLE"
    name                     = stringProperty "WM_NAME"
    web_shift                = ["Chromium", "Firefox", "luakit", "uzbl-core"]
    gimp_shift               = ["gimp-2.6", "Gimp-2.6", "Gimp", "gimp", "GIMP"]
    chat_shift               = ["Pidgin",   "Xchat"]
    center_floaters_by_class = ["MPlayer",  "zsnes", "Nvidia-settings", "XFontSel", "XCalc", "XClock", "gxmessage", "Xmessage", "Xwinmosaic"]
    center_floaters_by_name  = ["Config Video", "Testing plugin", "Config Sound", "Config Cdrom", "Config Bios", "Config Netplay", "Config Memcards", "About ePSXe", "Config Controller", "Config Gamepads", "Select one or more files to open", "Add media", "Choose a file", "Open Image", "File Operation Progress", "Firefox Preferences", "Preferences", "Search Engines", "Set up sync", "Passwords and Exceptions", "Autofill Options", "Rename File", "Copying files", "Moving files", "File Properties", "Replace", ""]
    move_to_side             = ["Event Tester"]
    avoid_focus              = ["Event Tester", "Notify-osd"]
    ignore_window_types      = ["desktop"]
    ignoramous               = ["kdestop", "plasma-desktop", "Plasma"]
