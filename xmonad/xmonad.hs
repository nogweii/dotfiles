import XMonad
import XMonad.Util.EZConfig
import DBus.Client.Simple
import XMonad.Hooks.DynamicLog
import qualified System.Taffybar.XMonadLog as TaffyLog

-- from ~/.xmonad/lib/
import Music.Pandora
import XMonad.Config.Evaryont.Keys
-- This does a lot of the real connecting
import XMonad.Config.Evaryont.Settings

-- Apply my keys on top of the default set (any conflicts? Mine win.)
final_settings = settings `additionalKeysP` key_bindings

main :: IO ()
main = do
       client <- connectSession -- Connect XMonad to DBus
       let pp = defaultPP
       xmonad final_settings { -- And...GO!
              logHook = TaffyLog.dbusLog client pp <+> logHook settings
       }
