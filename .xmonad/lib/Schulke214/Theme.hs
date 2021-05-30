module Schulke214.Theme where

import XMonad

import qualified Schulke214.Colors as C


data Theme = Theme
    { foreground :: String
    , background :: String
    , primary :: String
    , secondary :: String
    , border :: Dimension
    , gaps :: Integer
    }

-- Todo use default typeclass for Theme to set default gaps, borders, fg and bg colors!!

tokyoTheme :: Theme
tokyoTheme = Theme
    { foreground  = C.foreground
    , background  = C.background
    , primary     = C.magentaLight
    , secondary   = C.magenta
    , border      = 1
    , gaps        = 80
    }

vulcanTheme :: Theme
vulcanTheme = Theme
    { foreground  = C.foreground
    , background  = C.background
    , primary     = C.yellowLight
    , secondary   = C.redLight
    , border      = 1
    , gaps        = 80
    }

theme :: Theme
theme = tokyoTheme
