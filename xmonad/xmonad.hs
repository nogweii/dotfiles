import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
-- Used to connect to the dbus session bus, for xmonad-log-applet
import qualified DBus as DBus
import qualified DBus.Client as DBus

-- from ~/.xmonad/lib/
-- This does a lot of the real connecting
import XMonad.Config.Evaryont.Config
-- xmonad-screenshot, a neat-o tool that will take a screenshot of every
-- workspace in one go.
import XMonad.Util.WorkspaceScreenshot

main :: IO ()
main = do
       dbus <- DBus.connectSession
       getWellKnownName dbus
       -- Start the GTK event handler, for xmonad-screenshot
       initCapturing
       -- Run xmonad with my settings
       xmonad (evaryontConfig dbus)

getWellKnownName :: DBus.Client -> IO ()
getWellKnownName dbus = do
    DBus.requestName dbus (DBus.busName_ "org.xmonad.Log")
        [DBus.nameAllowReplacement, DBus.nameReplaceExisting, DBus.nameDoNotQueue]
    return ()
