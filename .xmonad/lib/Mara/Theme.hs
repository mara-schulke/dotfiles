module Mara.Theme where

import XMonad

import Data.Default

import qualified Mara.Colors as C


data Theme = Theme
    { foreground :: String
    , background :: String
    , primary :: String
    , secondary :: String
    , border :: Dimension
    , gaps :: Integer
    }

gapsLarge :: Theme -> Integer
gapsLarge t = 12 * gaps t

instance Default Theme where
    def = Theme fg bg fg bg 1 20
        where fg = C.foreground
              bg = C.background

natureTheme :: Theme
natureTheme = def
    { primary     = C.greenLight
    , secondary   = C.green
    }

tokyoTheme :: Theme
tokyoTheme = def
    { primary     = C.magentaLight
    , secondary   = C.magenta
    }

oceanTheme :: Theme
oceanTheme = def
    { primary     = C.blueLight
    , secondary   = C.blue
    }

vulcanTheme :: Theme
vulcanTheme = def
    { primary     = C.yellowLight
    , secondary   = C.redLight
    }

neutralTheme :: Theme
neutralTheme = def
    { primary     = C.whiteLight
    , secondary   = C.white
    }

theme :: Theme
theme = tokyoTheme
