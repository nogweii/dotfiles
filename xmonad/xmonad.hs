import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig

-- from ~/.xmonad/lib/
import Music.Pandora
import XMonad.Config.Evaryont.Keys

-- pretty much every piece of the config is in lib/XMonad/Config/Evaryont - go
-- check that out instead of this boring main function!
main = xmonad $ gnomeConfig {
       terminal           = "urxvt"
     , modMask            = mod4Mask
    } `additionalKeysP` key_bindings

--- vim: set syn=haskell nospell:
