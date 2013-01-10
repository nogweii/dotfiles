{-# LANGUAGE OverloadedStrings #-}

module XMonad.Config.Evaryont.Logger (
      appletLogger
      ) where

import XMonad
import qualified DBus as DBus
import qualified DBus.Client as DBus
import qualified Codec.Binary.UTF8.String as UTF8
import XMonad.Hooks.DynamicLog

appletLogger :: DBus.Client -> X ()
appletLogger dbus = dynamicLogWithPP (appletPrinter dbus)

appletPrinter :: DBus.Client -> PP
appletPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = const ""
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }

dbusOutput :: DBus.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (DBus.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
        DBus.signalBody = [DBus.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
    }
    DBus.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
