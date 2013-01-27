{-# LANGUAGE OverloadedStrings #-}

module XMonad.Config.Evaryont.Logger (
      appletLogger
      ) where

import XMonad
import qualified Codec.Binary.UTF8.String as UTF8
import XMonad.Hooks.DynamicLog

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
