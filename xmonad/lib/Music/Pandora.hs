-- Music.Pandora: XMonad control for pianobar (and therefore, pandora)
--
-- Copyright (C) 2011 Colin Shea <colin@evaryont.me>
-- Licensed under the GPL
module Music.Pandora (
      pandoraSelect
      ) where

import qualified Data.Map as M
-- Arch users: install haskell-xdg-basedir from AUR
import System.Environment.XDG.BaseDir
import System.IO
-- This is an xmonad module
import XMonad
import XMonad.Actions.GridSelect

-- custom navigation map that only lasts for one key press. Vim style is
-- used. (h/j/k/l)
pandoraNavigation :: TwoD a (Maybe a)
pandoraNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select) -- default: play/pause
         ,((0,xK_h) , move (-1,0) >> select) -- left: mark tired
         ,((0,xK_l) , move (1,0) >> select) -- right: skip
         ,((0,xK_j) , move (0,1) >> select) -- down: ban
         ,((0,xK_k) , move (0,-1) >> select) -- up: love
         ,((0,xK_space) , setPos (0,0) >> select) -- center: play/pause
         ]
       -- The navigation handler ignores unknown key symbols
       navDefaultHandler = const pandoraNavigation

-- configuration for gridSelect
pandoraGSConfig = defaultGSConfig {
      gs_cellheight = 30
    , gs_cellwidth  = 100
    , gs_navigate   = pandoraNavigation
}

-- The meat of the method: Write the passed paraemeter to pianobar's control
-- file
pianobarCmd :: String -> IO ()
pianobarCmd x = do path <- getUserConfigFile "pianobar" "ctl"
                   appendFile path x

-- The list of strings GridSelect shows, with their associated action
stringList = [("||", io $ (pianobarCmd "p")),
              ("X8", io $ (pianobarCmd "-")),
              (">>", io $ (pianobarCmd "n")),
              ("<3", io $ (pianobarCmd "+")),
              ("..", io $ (pianobarCmd "t"))]

-- The interface to this module. Add this as a keybind in XMonad
pandoraSelect = runSelectedAction pandoraGSConfig stringList
