import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog

-- from ~/.xmonad/lib/
-- This does a lot of the real connecting
import XMonad.Config.Evaryont.Config
-- xmonad-screenshot, a neat-o tool that will take a screenshot of every
-- workspace in one go.
import XMonad.Util.WorkspaceScreenshot

main :: IO ()
main = do
       -- Start the GTK event handler, for xmonad-screenshot
       initCapturing
       -- Run xmonad with my settings
       xmonad evaryontConfig
