import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig

-- from ~/.xmonad/lib/
import Music.Pandora

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
key_bindings = [ ("M-a",             pandoraSelect)
               , ("M-<Escape>",      kill)
               ]

main = xmonad $ gnomeConfig {
       terminal           = "urxvt"
     , modMask            = mod4Mask
     , keys               = keys gnomeConfig
    } `additionalKeysP` key_bindings

--- vim: set syn=haskell nospell:
