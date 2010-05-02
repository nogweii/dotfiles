import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Actions
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer

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

-- Prompts, baby, prompts!
import XMonad.Prompt
import XMonad.Prompt.AppendFile


-- The default number of workspaces (virtual screens) and their names.
--
the_workspaces    = ["1","2","3","4","5","6","7","8","9"]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
key_bindings = [ ("M-<Escape>", kill)
               , ("M-<Space>", sendMessage NextLayout)
               , ("M-r", refresh)
               , ("M-j", windows W.focusDown)
               , ("M-k", windows W.focusUp)
               , ("M-x", goToSelected grid_config)
               -- MPC keyboard control
               , ("<XF86AudioPlay>", spawn "exec mpc toggle")
               , ("<XF86AudioStop>", spawn "exec mpc stop")
               , ("<XF86AudioPrev>", spawn "exec mpc prev")
               , ("<XF86AudioNext>", spawn "exec mpc next")
               -- My keyboard (a G15) also includes volume controls, but KDE already
               -- manages some of them.
               -- For reference, the keys are <XF86AudioMute> <XF86AudioRaiseVolume> <XF86AudioLowerVolume>

               -- TODO: This will be replaced by a bashrun (but using zsh!) clone
               , ("M-g", appendFilePrompt defaultXPConfig "/home/colin/notes/notes.txt")

               -- mpc control via 'normal' keys
               , ("M-a l",       spawn "mpc next")
               , ("M-a h",       spawn "mpc prev")
               , ("M-a z",       spawn "mpc random")
               , ("M-a <Space>", spawn "mpc toggle")

               -- Spawn the configured terminal
               , ("M-<Return>", spawn $ XMonad.terminal the_settings)

               -- Toggle if a window is floating
               , ("M-t", withFocused (\windowId -> do { -- anonymous function
                           floats <- gets (W.floating . windowset); -- get all the floating windows
                           if windowId `M.member` floats -- check if the current window is in that list
                             then withFocused $ windows . W.sink -- 'sink' it if it is
                             else float windowId -- it's currently tiled, float it.
                         })
                 )
               ]

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
--
--    --
--    -- mod-[1..9], Switch to workspace N
--    --
--    -- mod-[1..9], Switch to workspace N
--    -- mod-shift-[1..9], Move client to workspace N
--    --
--    [((m .|. modm, k), windows $ f i)
--        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--    ++
--
--    --
--    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
--    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
--    --
--    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


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
manage_hook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Startup hook
--
-- Check the keymap for duplicates, errors, etc
--
startup_hook = return () >> checkKeymap the_settings key_bindings

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
the_settings = ewmh defaultConfig {
        terminal    = "urxvt.sh",
        modMask     = mod4Mask,
        workspaces  = the_workspaces,
        keys        = compiled_bindings,
        manageHook  = manage_hook,
        startupHook = startup_hook,
        logHook     = log_hook
}

--- vim: set syn=haskell nospell:
