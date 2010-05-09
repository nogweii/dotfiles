import XMonad
import XMonad.Config.Kde
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Actions
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleWS

-- Hooks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName

-- Utils
import XMonad.Util.EZConfig
import XMonad.Util.Replace
import XMonad.Util.Scratchpad

-- Prompts, baby, prompts!
import XMonad.Prompt
import XMonad.Prompt.AppendFile

-- The default number of workspaces (virtual screens) and their names.
--
the_workspaces    = ["1","2","3","4","5","6","7","8","9"]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
key_bindings = [
               -- Window switching
                 ("M-j",             windows W.focusDown)
               , ("M-k",             windows W.focusUp)
               , ("M-x",             goToSelected grid_config)
               , ("M-i",             moveTo Next NonEmptyWS)
               , ("M-o",             moveTo Prev NonEmptyWS)

               -- Window management
               , ("M-t",             toggleFloat)
               , ("M-<Escape>",      kill)
               , ("M-<Return>",      spawn $ XMonad.terminal the_settings)
               , ("M-u",             shiftTo Next EmptyWS)

               -- Note taking
               , ("M-g",             appendFilePrompt defaultXPConfig "/home/colin/notes/notes.txt")
               , ("M-S-g",           scratchpadSpawnActionTerminal "$HOME/bin/urxvt.sh")

               -- MPD Control
               -- Special multimedia keys
               , ("<XF86AudioPlay>", spawn "mpc toggle")
               , ("<XF86AudioStop>", spawn "mpc stop")
               , ("<XF86AudioPrev>", spawn "mpc prev")
               , ("<XF86AudioNext>", spawn "mpc next")
               -- Fall back
               , ("M-a l",           spawn "mpc next")
               , ("M-a h",           spawn "mpc prev")
               , ("M-a z",           spawn "mpc random")
               , ("M-a x",           spawn "dmenu-playlist.sh")
               , ("M-a <Space>",     spawn "mpc toggle")

               -- XMonad control
               , ("M-<Space>",       sendMessage NextLayout)
               , ("M-q",             spawn "xmonad --recompile; xmonad --restart")
               , ("M-S-q",           io (exitWith ExitSuccess))
               , ("M-r",             refresh)
               ]
               ++
               -- Workspaces: M-{1..9} - go to that work space, M-S-{1..9} - move client
               [(m ++ k, windows $ f w)
                    | (w, k) <- zip (XMonad.workspaces the_settings) (map show [1..9])
               , (m, f) <- [("M-",W.greedyView), ("M-S-",\w -> W.greedyView w . W.shift w)]]

               where toggleFloat = withFocused $ \windowId -> do
                       floats <- gets (W.floating . windowset)
                       if windowId `M.member` floats
                         then withFocused $ windows . W.sink
                         else float windowId

compiled_bindings = \c -> mkKeymap c $ key_bindings

--    -- launch dmenu
--    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
--
--    --  Reset the layouts on the current workspace to default
--    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
--
--    -- Resize viewed windows to the correct size
--    , ((modm,               xK_n     ), refresh)
--
--    -- Move focus to the master window
--    , ((modm,               xK_m     ), windows W.focusMaster  )
--
--    -- Swap the focused window and the master window
--    , ((modm,               xK_Return), windows W.swapMaster)
--
--    -- Swap the focused window with the next window
--    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
--
--    -- Swap the focused window with the previous window
--    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
--
--    -- Shrink the master area
--    , ((modm,               xK_h     ), sendMessage Shrink)
--
--    -- Expand the master area
--    , ((modm,               xK_l     ), sendMessage Expand)
--
--    -- Push window back into tiling
--    , ((modm,               xK_t     ), withFocused $ windows . (if M.member W.floating then W.float else W.sink))
--
--    -- Increment the number of windows in the master area
--    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
--
--    -- Deincrement the number of windows in the master area
--    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
--
--    -- Toggle the status bar gap
--    -- Use this binding with avoidStruts from Hooks.ManageDocks.
--    -- See also the statusBar function from Hooks.DynamicLog.
--    --
--    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
--
--    -- Quit xmonad
--    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
--
--    -- Restart xmonad
--    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
--    ]
--    ++

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
manage_hook = composeAll (

    -- Apps, etc to float & center
    [ className =? c <||> resource =? r <||> title =? t <||> isDialog --> doCenterFloat
    | c <- ["Wine", "Switch2", "quantum-Quantum"]
    , r <- ["Dialog", "Download"]
    , t <- ["Schriftart auswählen", "Choose a directory"]
    ] ++

    -- Separate float apps
    [ className =? "Plasma-desktop" --> doIgnore -- For KDE
    , className =? "kmix" --> doFloat -- For KDE
    , className =? "mplayer" --> doFloat
    , className =? "MPlayer" --> doFloat
    , className =? "Gimp" --> doFloat
    , title     =? "" --> doFloat
    , title     =? "x" --> doFloat -- weird cinelerra splash window
    , className =? "Cinelerra" --> doFloat

    -- Workspaces
    -- , className =? "Firefox"      --> makeMaster <+> moveTo 0
    -- , resource =? ""
    -- , title =? ""

    -- "Real" fullscreen
    , isFullscreen              --> doFullFloat
    , isDialog                  --> placeHook (inBounds (underMouse (0,0))) <+> makeMaster <+> doFloat
    -- RationalRect params: width, height, pos y, pos x -- in %
    , scratchpadManageHook (W.RationalRect 1.0 0.6 1.0 0.0)
    ]
    )

    -- Default hooks:
    -- <+> insertPosition Below Newer
    -- <+> positionStoreManageHook
    <+> manageDocks
    <+> makeMaster

  where makeMaster = insertPosition Master Newer

------------------------------------------------------------------------
-- Startup hook
--
-- Check the keymap for duplicates, errors, etc
--
startup_hook = return ()
            >> checkKeymap the_settings key_bindings
            >> setWMName "LG3D"

------------------------------------------------------------------------
-- Log hook
--
-- Fade unfocused windows to 90% transparency, and move the cursor to the center
-- of the newly focused window.
--
log_hook :: X ()
log_hook =  fadeInactiveLogHook 0.1
         >> updatePointer (Relative 0.5 0.5)

------------------------------------------------------------------------
-- Grid select configuration
--
-- Based off the default, add some extra keys.
--
-- TODO: Can I use scratchpadFilterOutWorkspace to filter out the scratchpad terminal?
grid_config = defaultGSConfig
    { gs_cellheight = 30
    , gs_cellwidth = 100
    , gs_navigate = M.unions
        [reset
        ,nethackKeys
        ,gs_navigate                                 -- get the default navigation bindings
            $ defaultGSConfig `asTypeOf` grid_config -- needed to fix an ambiguous type variable
        ]
    }
    where addPair (a,b) (x,y) = (a+x,b+y)
          nethackKeys = M.map addPair $ M.fromList
                              [((0,xK_y),(-1,-1))
                              ,((0,xK_i),(1,-1))
                              ,((0,xK_n),(-1,1))
                              ,((0,xK_m),(1,1))
                              ]
          -- jump back to the center with the spacebar, regardless of the current position.
          reset = M.singleton (0,xK_space) (const (0,0))

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
main = xmonad the_settings

-- A structure containing the configuration settings.
-- Any you don't override, will use the defaults defined in
-- xmonad/XMonad/Config.hs
--
the_settings = ewmh kde4Config {
        terminal    = "$HOME/bin/urxvt.sh",
        modMask     = mod4Mask,
        workspaces  = the_workspaces,
        keys        = compiled_bindings,
        manageHook  = manage_hook,
        startupHook = startup_hook,
        logHook     = log_hook
}

------------------------------------------------------------------------
-- Notes
--
-- Get the screen width
-- gets $ screenRect . W.screenDetail . W.current . windowset

--- vim: set syn=haskell nospell:
