module XMonad.Config.Evaryont.Keys (
      key_bindings,
      toggleStrutsKey
      ) where

import Music.Pandora
import System.Environment
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Config.Evaryont.Logout
import XMonad.Config.Evaryont.Settings
import XMonad.Config.Evaryont.Utils
import XMonad.Darcs.Util.EZConfig
import XMonad.Darcs.Util.NamedActions
import XMonad.Util.WorkspaceScreenshot

import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.CopyWindow (kill1)
import System.Exit

-- Key bindings. Add, modify or remove key bindings here.
--
--key_bindings = 
--   [
--    ("M-d a", addName "useless message" $ spawn "xmessage foo"),
--    ("M-c", sendMessage' Expand)]
--   , ("M-<Escape>",    addName "Close the current window"             $ kill)
--   , ("M-S-<Escape>",  addName "Close target window"                  $ spawn "xkill")
--   , ("M-q",           addName "Lock the screen"                      $ spawn lockScreen)
--   , ("M-S-q",         addName "Restart XMonad"                       $ restart "xmonad" True)
--   , ("M-C-q",         addName "Magic KDE dialog"                     $ logoutDialog)
--   , ("M-S-p",         addName "Take a screenshot (gnome-panel)"      $ spawn "gnome-panel-screenshot -i")
--   , ("M-t",           addName "Toggle current window's float status" $ toggleFloat)
--   , ("M-?",           addName "Show the configured keys"             $ spawn "show-xmonad-keys")
--   , ("M-S-?",         addName "Show the configured keys"             $ spawn "show-xmonad-keys")
--   ]


key_bindings conf =
    mkNamedKeymap conf $
    [ subtitle' "Killing & Restarting"
    , ("M-<Escape>", addName "Kill current window" $ kill1)
    , ("M-S-<Escape>", addName "Kill all windows on workspace" $ killAll)
    , ("M-C-<Escape>", addName "Restart XMonad" $ spawn "xmonad --restart")
    , ("M-S-C-<Escape>", addName "Quit XMonad" $ io (exitWith ExitSuccess))

    , subtitle' "Application launching"
    , ("M-<Enter>", addName "Launch terminal" $ spawn terminal_choice)
    , ("M-p", addName "Run dmenu to launch an application" $ spawn "~/bin/dmenu-run")
    , ("M-x", addName "Switch to another window" $ spawn "xwinmosaic")

    , subtitle' "Workspace movement"
    , ("M-l",           addName "Go to next non-empty workspace"       $ moveTo Next NonEmptyWS)
    , ("M-h",           addName "Go to previous non-empty workspace"   $ moveTo Prev NonEmptyWS)
    , ("M-S-l",         addName "Go to next empty workspace"           $ moveTo Next EmptyWS)
    , ("M-S-h",         addName "Go to previous empty workspace"       $ moveTo Prev EmptyWS)
    , ("M-<Backspace>", addName "Toggle the last workspace you're on"  $ toggleWS)

    , subtitle' "Music control"
    , ("M-a",           addName "Music control (pandora only)"         $ pandoraSelect)
    , ("<XF86AudioPlay>", addName "Toggle MPD" $ spawn "mpc toggle")
    , ("<XF86AudioNext>", addName "Next song in the playlist" $ spawn "mpc next")

    , subtitle' "Uncategorized"
    , ("M-e",           addName "Go to next Xinerama screen"           $ nextScreen)
    , ("M-<Print>",     addName "Screenshot every workspace"           $ captureWorkspacesWhen defaultPredicate captureHook horizontally)
    ]

subtitle' :: String -> (String, NamedAction)
subtitle' str = ([], NamedAction $ str ++ ":")

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- vim: set nospell:
