import XMonad
import XMonad.Config.Kde
-- to shift and float windows
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place

-- Utils
import XMonad.Util.EZConfig
import XMonad.Util.Replace
import XMonad.Util.Scratchpad

main = do
    replace
    xmonad $ ewmh kde4Config {

        modMask = mod4Mask -- use the Windows button as mod
      , terminal = "urxvt.sh"

      , keys = myKeys

      , manageHook = manageHook kdeConfig <+> myManageHook
      , logHook = myLogHook

    }

myManageHook = composeAll (

    -- Apps, etc to float & center
    [ className =? c <||> resource =? r <||> title =? t <||> isDialog --> doCenterFloat
    | c <- ["Wine", "Switch2", "quantum-Quantum"]
    , r <- ["Dialog", "Download"]
    , t <- ["Schriftart auswählen", "Choose a directory"]
    ] ++

    -- Separate float apps
    [ className =? "Plasma-desktop" --> doFloat -- For KDE

    -- Workspaces
    -- , className =? "Firefox"      --> makeMaster <+> moveTo 0
    -- , resource =? ""
    -- , title =? ""

    -- "Real" fullscreen
    , isFullscreen              --> doFullFloat
    , isDialog                  --> placeHook (inBounds (underMouse (0,0))) <+> makeMaster <+> doFloat
    ] )

    -- Default hooks:
    -- <+> insertPosition Below Newer
    -- <+> positionStoreManageHook
    <+> manageDocks
    -- TODO: Figure out the rectangle required for a 1-line terminal. (Note: percentages!)
    <+> scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.35)

  where makeMaster = insertPosition Master Newer


myKeys conf = mkKeymap conf $
    [ ("M-<Escape>", kill)
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-r", refresh)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    -- MPC keyboard control
    , ("<XF86AudioPlay>", spawn "exec mpc toggle")
    , ("<XF86AudioStop>", spawn "exec mpc stop")
    , ("<XF86AudioPrev>", spawn "exec mpc prev")
    , ("<XF86AudioNext>", spawn "exec mpc next")
    -- My keyboard (a G15) also includes volume controls, but KDE already
    -- manages them.
    -- For reference, the keys are <XF86AudioMute> <XF86AudioRaiseVolume> <XF86AudioLowerVolume>

    -- TODO: This will be replaced by a bashrun (but using zsh!) clone
    , ("M-g", scratchpadSpawnActionTerminal "urxvt" )
    ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.8

--- vim: set syn=haskell nospell:
