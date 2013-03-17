module XMonad.Config.Evaryont.Layout (
      layout_hook,
      ) where

import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ShowWName

import Data.List  ((\\))
import Data.Ratio ((%))

import XMonad.Config.Evaryont.Settings (iconspaces)

-- The size of the 'sidebar' in whatever layout it's part of. e.g. the size of
-- the gimp toolbox, or buddy list from Pidgin
magicRatio = 1%7

--layout_hook = -- The following modify every workspace
--              modWorkspaces    iconspaces          avoidStruts                         |||
--              modWorkspaces    iconspaces          layoutHintsWithPlacement (0.5, 0.5) |||
--              modWorkspaces    iconspaces          smartBorders                        |||
--              -- Only put spacing on every workspace *except* the browsing workspace
--              modWorkspacesBut [(iconspaces !! 1)] smartSpacing             5          |||
--              -- And on the browsing workspace, *only* use Tabbed
--              onWorkspace      (iconspaces !! 1)   simpleTabbed                        |||
--              layoutHook kde4Config
--              where

--layout_hook = avoidStruts $ layoutHintsWithPlacement (0.5, 0.5) $ smartBorders $
--              onWorkspace   (iconspaces !! 1) fullscreenFull $
--              modWorkspace  (iconspaces !! 4) smartBorders $ im $
----            modWorkspaces (iconspaces \\ (iconspaces !! 1)) smartSpacing 5 $
--              Full ||| Grid -- The layouts here are the defaults from left->right
--                where im = withIM (1%7) (Role "buddy_list")


--modWorkspacesBut :: (LayoutClass l a)
--                 => [WorkspaceId]
--                 -> (l a -> ModifiedLayout lm l a)
--                 -> l a
--                 -> PerWorkspace (ModifiedLayout lm l) l a
--modWorkspacesBut excludedWorkspaces f l = PerWorkspace excludedWorkspaces False (f l) l


--layout_hook = avoidStruts $ layoutHintsWithPlacement (0.5, 0.5) $ smartBorders $ Grid
--layout_hook = Grid

gimpLayout = withIM magicRatio (Role "gimp-toolbox") $
             reflectHoriz Full

layout_hook = showWName $ smartSpacing 5 $ avoidStruts $ layoutHintsWithPlacement (0.5, 0.5) $ smartBorders $
              onWorkspace  (iconspaces !! 1) simpleTabbed $
              onWorkspace  (iconspaces !! 4) im
              standardLayouts
              where
                im              = withIM magicRatio (Role "buddy_list") standardLayouts
                standardLayouts = Grid ||| Full
