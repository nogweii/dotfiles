-- XMonad.Config.Evaryont.Logout: A grid select based log out dialog, for KDE
--
-- Copyright (C) 2011 Colin Shea <colin@evaryont.me>
-- Licensed under the GPL
module XMonad.Config.Evaryont.Logout (
      logoutDialog,
      lockScreen
      ) where

import qualified Data.Map as M
import System.IO
-- This is an xmonad module
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Util.Run
import XMonad.Config.Evaryont.Utils

sessionManager :: String -> String
sessionManager x = dbusCommand "org.freedesktop.PowerManagement"
                               "/org/freedesktop/PowerManagement"
                               ("org.freedesktop.PowerManagement." ++ x)

kdeSession :: String -> String
kdeSession "logout" = dbusCommand "org.kde.ksmserver"
                                   "/KSMServer"
                                   "org.kde.KSMServerInterface.logout 0 0 -1"
kdeSession "reboot" = dbusCommand "org.kde.ksmserver"
                                  "/KSMServer"
                                  "org.kde.KSMServerInterface.logout 0 1 -1"
kdeSession "shutdown" = dbusCommand "org.kde.ksmserver"
                                    "/KSMServer"
                                    "org.kde.KSMServerInterface.logout 0 2 -1"

lockScreen :: String
lockScreen = dbusCommand "org.freedesktop.ScreenSaver" "/ScreenSaver" "Lock"


-- The list of strings GridSelect shows, with their associated action
stringList = [("Logout",         unsafeSpawn (kdeSession "logout")),
              ("Hibernate",      unsafeSpawn (sessionManager "Hibernate")),
              ("Suspend",        unsafeSpawn (sessionManager "Suspend")),
              ("Reboot",         unsafeSpawn (kdeSession "reboot")),
              ("Restart XMonad", restart "xmonad" True),
              ("Lock Screen",    unsafeSpawn (lockScreen)),
              ("Shutdown",       unsafeSpawn (kdeSession "shutdown"))]

-- The interface to this module. Add this as a keybind in XMonad
logoutDialog = runSelectedAction defaultGSConfig stringList
