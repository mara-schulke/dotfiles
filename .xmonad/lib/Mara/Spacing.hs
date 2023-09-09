module Mara.Spacing where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing

import qualified Mara.Theme as T


spacing :: l a -> ModifiedLayout Spacing l a
spacing = spacing' $ T.gaps T.theme

spacing' :: Integer -> l a -> ModifiedLayout Spacing l a
spacing' i = spacingRaw False (border i) True (border 0) False

border :: Integer -> Border
border i = Border i i i i
