module Schulke214.Spacing where


import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing

import qualified Schulke214.Theme as T


spacing :: l a -> ModifiedLayout Spacing l a
spacing = spacing' $ T.gaps T.theme

spacing' :: Integer -> l a -> ModifiedLayout Spacing l a
spacing' i = spacingRaw False (Border i i i i) True (Border i i i i) True

