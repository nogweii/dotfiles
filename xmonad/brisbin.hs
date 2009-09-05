--
-- ~/.xmonad/xmonad.hs by pbrisbin
--
-- darcs and contrib required for most of this
--
-- read the commments before you M-q
--
-- Last updated: 3 september 2009
--

-- Imports {{{
--
-- i try to import only what i use, therefore if
-- you make edits, then get out of scope errors,
-- try removing the parts in parenthesis so you
-- get the whole module at your disposal in stead
--
import XMonad

import XMonad.Actions.RotSlaves     (rotSlavesUp, rotSlavesDown)
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WithAll       (killAll)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.IM
import XMonad.Layout.LayoutHints    (layoutHintsWithPlacement)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace   (onWorkspace)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane

import XMonad.Util.Loggers          (maildirNew, wrapL)
import XMonad.Util.Run              (spawnPipe)
import XMonad.Util.WindowProperties (getProp32s)

import Data.List                    (isInfixOf)
import Data.Monoid
import Data.Ratio

import System.IO
import System.Exit

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

-- not sure i really need these...
import Foreign.C.Types (CLong)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

-- }}}

-- Main {{{
-- 
-- spawns the two statusbars and xmonad with all of the
-- customizations defined throughout this xmonad.sh
--
main = do
  d <- spawnPipe myLeftBar
  spawn myRightBar
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal           = myTerminal
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , keys               = myKeys
    , logHook            = myLogHook d
    }

-- }}}

-- Theme {{{
--
-- adjust these variables, they are used throughout
--
myXFTFont    = "xft:Verdana-8"   -- see 'Status Bars' for dzen font
conkyFile    = "~/.dzen_conkyrc" -- populates right status bar

colorBG      = "#303030"         -- background
colorFG      = "#606060"         -- foreground
colorFG2     = "#909090"         -- foreground w/ emphasis
colorFG3     = "#ffffff"         -- foreground w/ strong emphasis
colorUrg     = "#ff0000"         -- urgent

barHeight    = 17
monitorWidth = 1920              -- two statusbars will span this width
leftBarWidth = 700               -- right bar will span difference

-- }}}

-- Options {{{
--
-- you should adjust the terminal
--
-- if you change workspace names, be sure to update them throughout
--
myTerminal           = "urxvtc"
myWorkspaces         = ["1-irssi", "2-web" ,"3-im", "4-terms"] ++ map show [5..9]
myNormalBorderColor  = colorBG
myFocusedBorderColor = colorFG3
myBorderWidth        = 2

-- }}}

-- Layouts {{{
--
-- xmonad darcs 0.8.0.1 required for layoutHintsWithPlacement
--
myLayout = avoidStruts $ onWorkspace "2-web" webLayouts $ onWorkspace "3-im" imLayout $ standardLayouts
  where
    standardLayouts = tiled ||| Mirror tiled ||| tabLayout ||| full
    webLayouts      = tabLayout ||| twoPane ||| full
    imLayout        = withIM (1/10) (Role "roster") (standardLayouts)

    tiled           = hintTopLeft (ResizableTall nmaster delta ratio [])
    twoPane         = hintTopLeft (TwoPane delta (1/2))
    tabLayout       = hintTopLeft (tabbedBottom shrinkText myTabConfig)
    full            = hintTopLeft (noBorders Full)
   
    -- accept a layout, return a hinted layout
    hintTopLeft l   = layoutHintsWithPlacement (0, 0) l
    hintCenter l    = layoutHintsWithPlacement (0.5, 0.5) l

    nmaster         = 1
    delta           = 3/100
    ratio           = toRational (2/(1 + sqrt 5 :: Double)) -- golden ratio

-- custom tab bar theme
myTabConfig :: Theme
myTabConfig = defaultTheme
  { fontName            = myXFTFont
  , activeColor         = colorBG
  , inactiveColor       = colorBG
  , urgentColor         = colorBG
  , activeBorderColor   = colorFG2
  , inactiveBorderColor = colorFG
  , urgentBorderColor   = colorFG3
  , activeTextColor     = colorFG3
  , inactiveTextColor   = colorFG2
  , urgentTextColor     = colorUrg
  , decoHeight          = 20
  }

-- }}}

-- ManageHook {{{
--
-- xmonad < 0.8.1 means you'll just need more parens:
--   e.g. [(class x <&&> role y) --> (doSomething)]
--
myManageHook = (composeAll . concat $
  [ [resource   =? d                --> doIgnore             | d      <- myIgnores ]
  , [className  =? m <&&> role =? r --> ask >>= doF . W.sink | (m, r) <- [myIM]    ]
  , [title      =? i                --> doShift "1-irssi"    | i      <- myIRC     ]
  , [className  =? w                --> doShift "2-web"      | w      <- myWebs    ]
  , [className  =? m                --> doShift "3-im"       | m      <- [fst myIM]]
  , [className  =? f                --> doFloat              | f      <- myFloats  ]
  ]) <+> manageTypes <+> manageDocks
  where
    role      = stringProperty "WM_WINDOW_ROLE"

    myIM      = ("Gajim.py","roster") -- ("Class","Role")

    myIRC     = ["irssi"]
    myWebs    = ["Navigator", "Shiretoko", "Firefox", "Uzbl"]
    myIgnores = ["desktop", "desktop_window"]
    myFloats  = [fst myIM] ++ ["MPlayer", "Zenity", "VirtualBox", "Xmessage", "Save As...", "XFontSel"]

-- modified version of manageDocks
manageTypes :: ManageHook
manageTypes = checkType --> doFloat

checkType :: Query Bool
checkType = ask >>= \w -> liftX $ do
  m   <- getAtom    "_NET_WM_WINDOW_TYPE_MENU"
  d   <- getAtom    "_NET_WM_WINDOW_TYPE_DIALOG"
  u   <- getAtom    "_NET_WM_WINDOW_TYPE_UTILITY"
  mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w

  case mbr of
    Just [r] -> return $ elem (fromIntegral r) [m, d, u]
    _        -> return False

-- }}}

-- Status Bars {{{
--
-- use a custom function to build two dzen2 bars
--

-- this way it's just your xft font minus the "xft:" part;
-- this requires an xft enabled dzen, if you don't want to
-- do that, just replace this with some "-*-*-*-whatever"
-- font definition
myDzenFont :: String
myDzenFont = drop 4 myXFTFont

makeDzen :: Int -> Int -> Int -> Int -> String -> String
makeDzen x y w h a = "dzen2 -p" ++
                     " -ta "    ++ a          ++
                     " -x "     ++ show x     ++
                     " -y "     ++ show y     ++
                     " -w "     ++ show w     ++
                     " -h "     ++ show h     ++
                     " -fn '"   ++ myDzenFont ++ "'" ++
                     " -fg '"   ++ colorFG    ++ "'" ++
                     " -bg '"   ++ colorBG    ++ "' -e ''"

-- define the bars
myLeftBar   = makeDzen 0 0 leftBarWidth barHeight "l"
myRightBar  = "conky -c " ++ conkyFile ++ " | " ++ makeDzen leftBarWidth 0 (monitorWidth - leftBarWidth) barHeight "r"

-- }}}

-- LogHook {{{
myLogHook :: Handle -> X ()
myLogHook h = (dynamicLogWithPP $ defaultPP
  { ppCurrent         = dzenColor colorBG  colorFG2 . pad
  , ppHidden          = dzenFG    colorFG2 . pad
  , ppHiddenNoWindows = dzenFG    colorFG  . pad
  , ppLayout          = dzenFG    colorFG  . myRename
  , ppUrgent          = myWrap    colorUrg "{"  "}"  . pad
  , ppTitle           = myWrap    colorFG2 "[ " " ]" . shorten 40
  , ppExtras          = [myMail]
  , ppWsSep           = ""
  , ppSep             = " "
  , ppOutput          = hPutStrLn h
  }) >> updatePointer (Relative 0.95 0.95) >> myFadeInactive 0.70
  where
    dzenFG  c     = dzenColor c ""
    myWrap  c l r = wrap  (dzenFG c l) (dzenFG c r)
    myWrapL c l r = wrapL (dzenFG c l) (dzenFG c r) -- wrapL needed for loggers

    myMail = myWrapL colorFG2 "Mail: " "" $ maildirNew "/home/patrick/Mail/GMail/INBOX"

    myRename = (\x -> case x of
               "Hinted ResizableTall"           -> "Tall"
               "Mirror Hinted ResizableTall"    -> "Wide"
               "Tabbed Bottom Simplest"         -> "Tabbed"
               "Hinted TwoPane"                 -> "TwoPane"
               "Hinted Full"                    -> "Full"
               "IM Hinted ResizableTall"        -> "IM Tall"
               "IM Mirror Hinted ResizableTall" -> "IM Wide"
               "IM Tabbed Bottom Simplest"      -> "IM Tabbed"
               "IM Full"                        -> "IM Full"
               _                                -> x
               )

-- }}}

-- FadeInactive *HACK* {{{
--
-- you can probably just use fadeInactiveLogHook
--
-- this is a rewrite that checks layout, because xcompmgr is
-- epic fail with this on any full and tabbed layouts for me
--

-- sets the opacity of inactive windows to the specified amount
-- unless the current layout is full or tabbed
myFadeInactive :: Rational -> X ()
myFadeInactive = fadeOutLogHook . fadeIf (isUnfocused <&&> isGoodLayout)

-- returns True if the layout is not Full or Tabbed
isGoodLayout:: Query Bool
isGoodLayout = liftX $ do
  l <- gets (description . W.layout . W.workspace . W.current . windowset)
  return $ not $ any (`isInfixOf` l) ["Full", "Tabbed"]

-- }}}

-- Key Bindings {{{
--
-- only those which override/change defaults
--
myKeys = \c -> keyBinds c `M.union` keys defaultConfig c
  where
    keyBinds (XConfig {modMask = modm}) = M.fromList $
      [ ((modm,                 xK_p        ), spawn "launch.sh"       ) -- %! dmenu wrapper, sort apps by usage -- *custom script*
      , ((modm .|. shiftMask,   xK_u        ), spawn myBrowser         ) -- %! open web client
      , ((modm .|. shiftMask,   xK_m        ), spawn myMail            ) -- %! open mail client
      , ((modm .|. shiftMask,   xK_i        ), spawn myIRC             ) -- %! open IRC client
      , ((modm,                 xK_a        ), spawn "msearch all"     ) -- %! Search current playlist via dmenu -- *custom script*
      , ((modm,                 xK_g        ), spawn "goodsong"        ) -- %! Note current song as 'good'       -- *custom script*
      , ((modm .|. shiftMask,   xK_g        ), spawn "goodsong -p"     ) -- %! Play a random 'good' song         -- *custom script*
      , ((modm,                 xK_w        ), kill                    ) -- %! Close currently focused window    -- *nonstandard*
      , ((modm .|. shiftMask,   xK_w        ), killAll                 ) -- %! Close all windows on current ws   -- *import required*
      , ((modm .|. controlMask, xK_k        ), rotSlavesUp             ) -- %! Rotate slave windows up           -- *import required*
      , ((modm .|. controlMask, xK_j        ), rotSlavesDown           ) -- %! Rotate slave windows down         -- *import required*
      , ((modm,                 xK_o        ), sendMessage MirrorShrink) -- %! Shink slave panes vertically      -- *import required*
      , ((modm,                 xK_i        ), sendMessage MirrorExpand) -- %! Expand slave panes vertically     -- *import required*
      , ((modm,                 xK_BackSpace), focusUrgent             ) -- %! Focus most recently urgent window -- *import required*
      , ((modm .|. shiftMask,   xK_BackSpace), clearUrgents            ) -- %! Make urgents go away              -- *import required*
      , ((modm,                 xK_q        ), spawn myRestart         ) -- %! Restart xmonad                    -- *see below*
      ]

    myBrowser = "uzbl"
    myMail    = myTerminal ++ " -e mutt"
    myIRC     = myTerminal ++ " -e irssi"

    -- mmm... must... kill... zombies...
    myRestart = "for pid in `pgrep conky`; do kill -9 $pid; done && " ++ -- kill running conky's,
                "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++ -- kill running dzen's, then
                "xmonad --recompile && xmonad --restart"                 -- issue default restart command

-- }}}

--
-- vim:foldmethod=marker
--
