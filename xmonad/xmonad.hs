-- Sources of configuration:
-- https://haskell.org/haskellwiki/Xmonad/Config_archive/lorincs_xmonad.hs
-- https://haskell.org/haskellwiki/Xmonad/Config_archive/loupgaroublonds_xmonad.hs
-- https://haskell.org/haskellwiki/Xmonad/Config_archive/31d1's_xmonad.hs
-- https://haskell.org/haskellwiki/Xmonad/Config_archive/lazor's_xmonad.hs
-- https://haskell.org/haskellwiki/Xmonad/Config_archive/Herzen's_xmonad.hs
-- https://haskell.org/haskellwiki/Xmonad/Using_xmonad_in_KDE
--
-- And as Daft Punk says, around the world...
import XMonad
import IO
import XMonad.Config.Kde
import Data.Monoid
import System.Exit

-- import DBus
-- import DBus.Connection
-- import DBus.Message

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Actions
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleWS

-- Hooks
import XMonad.Hooks.DynamicLog
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

---- This retry is really awkward, but sometimes DBus won't let us get our
---- name unless we retry a couple times.
--getWellKnownName :: Connection -> IO ()
--getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) ->
--                        where
--                         tryGetName = do
--                           namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
--                           addArgs namereq [String "org.xmonad.Log", Word32 5]
--                           sendWithReplyAndBlock dbus namereq 0
--                           return ()


-- The default number of workspaces (virtual screens) and their names.
--
the_workspaces    = ["1","2","3","4","5","6","7","8","9"]

icons_path = "/home/colin/.icons"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
key_bindings = [
               -- Window switching
                 ("M-j",             windows W.focusDown)
               , ("M-k",             windows W.focusUp)
               , ("M-S-j",           windows W.swapDown)
               , ("M-S-k",           windows W.swapUp)
               , ("M-S-<Return>",    windows W.swapMaster)
               , ("M-x",             goToSelected grid_config)
               , ("M-i",             moveTo Next NonEmptyWS)
               , ("M-o",             moveTo Prev NonEmptyWS)
               , ("M-h",             sendMessage Shrink)
               , ("M-l",             sendMessage Expand)

               -- Window management
               , ("M-t",             toggleFloat)
               , ("M-<Return>",      spawn $ XMonad.terminal the_settings)
               , ("M-u",             shiftTo Next EmptyWS)
               -- increase or decrease number of windows in the master area
               , ("M-,",             sendMessage (IncMasterN 1))
               , ("M-.",             sendMessage (IncMasterN (-1)))

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

               -- Volume control
               , ("<XF86AudioRaiseVolume>", spawn "dvol -i 2")
               , ("<XF86AudioLowerVolume>", spawn "dvol -d 2")
               , ("M-a k",           spawn "dvol -i 2")
               , ("M-a j",           spawn "dvol -d 2")

               -- XMonad control
               , ("M-<Space>",       sendMessage NextLayout)
               -- IDEA: Pop up an OSD with the new layout's name
   -- -- Rotate through layouts
   -- , ((modMask,               xK_grave ), sendMessage NextLayout
   -- >> (dynamicLogString myPP >>= \d->safeSpawn "gnome-osd-client" [d]))
               , ("M-q",             spawn "xmonad --recompile; xmonad --restart")
               , ("M-S-q",           io (exitWith ExitSuccess))

    -- [ ((modm, xK_p), spawn "krunner")
    -- , ((modm .|. shiftMask, xK_q), spawn "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1")
    -- , ((modm, xK_a), withFocused (sendMessage . expandWindowAlt))

               , ("M-r",             refresh)
               , ("M-f",             spawn "qdbus org.freedesktop.ScreenSaver /ScreenSaver Lock")

                   -- toggle focused window fullscreen
                   --     , ((modMask,               xK_m     ), sendMessage (Toggle "Full"))
                   --         >> (dynamicLogString myPP >>= \d->safeSpawn "gnome-osd-client" [d]))"

               -- Applications!
               , ("M-<Escape>",      kill)
               , ("M-p",             spawn "dmenu_run")
               , ("M-S-p",           spawn "dmenu-app")
               , ("M-b",             spawn "keynav \"start, grid 2x2\"")
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

--
--    --  Reset the layouts on the current workspace to default
--    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
--
--    -- Toggle the status bar gap
--    -- Use this binding with avoidStruts from Hooks.ManageDocks.
--    -- See also the statusBar function from Hooks.DynamicLog.
--    --
--    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
--
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
    [ -- className =? "Plasma-desktop" --> doIgnore, -- For KDE
      className =? "mplayer" --> doFloat
    , className =? "MPlayer" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "Cinelerra" --> doFloat
    , className =? "stalonetray" --> doIgnore
    -- x11-ssh-askpass: Float it
    , className =? "x11-ssh-askpass" --> doFloat
    , className =? "ssh-askpass" --> doFloat
    , className =? "SshAskpass" --> doFloat
    -- OpenOffice Table Toolbar
    , className =? "VCLSalFrame" --> doFloat
    , title     =? "Namoroka Preferences" --> doFloat
    , className =? "sun-awt-X11-XDialogPeer" --> doFloat
    , className =? "sun-applet-PluginMain" --> doFloat
    , className =? "sun-awt-X11-XFramePeer" --> doFloat
    , className =? "net-minecraft-MinecraftLauncher" --> doFloat
    , className =? "Eclipse" --> doFloat
    , resource  =? "stalonetray" --> doIgnore
    , className =? "xawtv" --> doFloat

    -- Workspaces
    -- , className =? "Firefox"      --> makeMaster <+> moveTo 0
    -- , resource =? ""
    -- , title =? ""

    -- "Real" fullscreen
    , isFullscreen              --> doFullFloat
    , isDialog                  --> placeHook (inBounds (underMouse (0,0))) <+> makeMaster <+> doFloat
    , isKDETrayWindow           --> doIgnore
    -- RationalRect params: width, height, pos y, pos x -- in %
    , scratchpadManageHook (W.RationalRect 1.0 0.6 1.0 0.0)
    , transience'
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
log_hook =  fadeInactiveLogHook 0.6
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

-- myPP handle = defaultPP {
--         ppCurrent = wrap ("^fg(" ++ fgHLight color_scheme ++ ")^bg(" ++ bgHLight color_scheme ++ ")^p(4)") "^p(4)^fg()^bg()",
--         -- Inverse the hilight colors for urgency
--         ppUrgent = wrap ("^fg(" ++ bgHLight color_scheme ++ ")^bg(" ++ fgHLight color_scheme ++ ")^p(4)") "^p(4)^fg()^bg()",
--         ppVisible = wrap ("^fg(" ++ fgColor color_scheme ++ ")^bg(" ++ bgColor color_scheme ++ ")^p(4)") "^p(4)^fg()^bg()",
--         ppSep     = "^fg(" ++ borderColor color_scheme ++ ")^r(3x3)^fg()",
--         ppTitle   = wrap ("^fg(" ++ bgHLight  ++ ")") "^fg()" ,
--         ppOutput  = hPutStrLn handle
-- }
-- 
myDzenPP h = defaultPP {
                        ppOutput          = hPutStrLn h,
                        ppSep             = (wrapFg  (borderColor color_scheme) "^r(3x3)"),
                        ppVisible         = wrapFgBg (fgColor color_scheme) (bgColor color_scheme),
                        ppCurrent         = wrapFgBg (fgHLight color_scheme)    (bgHLight color_scheme),
                        -- Inverse the hilight colors for urgency
                        ppUrgent          = wrapFgBg (bgHLight color_scheme) (fgHLight color_scheme),
                        ppTitle           = (\x -> "  " ++ wrapFg (fgHLight color_scheme) x),
                        ppLayout          = (\x -> case x of
                                              "ResizableTall"        -> wrapIcon "dzen_bitmaps/tall.xbm"
                                              "Mirror ResizableTall" -> wrapIcon "dzen_bitmaps/mtall.xbm"
                                              "Full"                 -> wrapIcon "dzen_bitmaps/full.xbm"
                                              _                      -> " " ++ x ++ " "
                                            )
                       }
                        where
                          wrapFgBg fgColor bgColor content= wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
                          wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
                          wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
                          wrapIcon path = " ^i(" ++ icons_path ++ "/" ++ path ++ ")"

-- Universal color scheme. DZen, prompts, everything!
color_scheme = defaultXPConfig {
                   font              = "-*-liberation mono-medium-r-*-*-12-*-*-*-*-*-*-*"
                 , bgColor           = "#2f2f2f"
                 , fgColor           = "#0099cc"
                 , bgHLight          = "#aecf96"
                 , fgHLight          = "black"
                 , borderColor       = "blue"
                 }

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
-- IDEA: Register xmonad to GNOME's session manager. XXX: What about KDE's KSM?
--  env <- getEnvironment
--  case lookup "DESKTOP_AUTOSTART_ID" env of
--      Just id -> do
--          forkIO $ (>> return ()) $ rawSystem "dbus-send" ["--session","--print-reply=string","--dest=org.gnome.SessionManager","/org/gnome/SessionManager","org.gnome.SessionManager.RegisterClient","string:xmonad","string:"++id]
--          return ()
--      Nothing -> return ()
-- main = withConnection Session $ \ dbus -> do
--   putStrLn "Getting well-known name."
--   getWellKnownName dbus
--   putStrLn "Got name, starting XMonad."
--   xmonad $ gnomeConfig
main = xmonad the_settings

-- A structure containing the configuration settings.
-- Any you don't override, will use the defaults defined in
-- xmonad/XMonad/Config.hs
--
the_settings = ewmh kde4Config {
        --terminal    = "$HOME/bin/urxvt.sh",
        terminal    = "urxvt",
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
