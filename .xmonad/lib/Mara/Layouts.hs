module Mara.Layouts where

import XMonad
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook)
import XMonad.Layout
import XMonad.Layout.LayoutModifier
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen
import XMonad.Layout.Renamed
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger (windowArrange)

import qualified Mara.Spacing as S
import qualified Mara.Theme as T

import XMonad.Layout.NoFrillsDecoration

deco = noFrillsDeco shrinkText def
    { activeColor         = T.primary T.theme
    , inactiveColor       = T.background T.theme
    , activeTextColor     = T.background T.theme
    , activeBorderWidth   = 0
    , inactiveBorderWidth = 0
    , urgentBorderWidth   = 0
    }


layoutHook =
    avoidStruts
    $ refocusLastLayoutHook
    $ mouseResize
    $ windowArrange
    $ windowNavigation
    $ toggleLayouts (noBorders $ full)
    $ lessBorders OnlyScreenFloat
    $ layouts
    where layouts            = gridGaps ||| tallGaps ||| tallMirrorGaps ||| tcGaps ||| tcMirrorGaps ||| full

          tallRaw            = reflectVert $ ResizableTall 1 (3/100) (1/2) []
          tallGaps           = variant "tall" "" $ withGaps tallRaw
          tallMirrorGaps     = variant "tall" "mirror" $ withGaps $ Mirror tallRaw

          gridRaw            = GridRatio (4/3)
          gridGaps           = variant "grid" "" $ withGaps gridRaw

          tcRaw              = ThreeColMid 1 (3/100) (1/2)
          tcGaps             = variant "threecol" "" $ withGaps tcRaw
          tcMirrorGaps       = variant "threecol" "mirror" $ withGaps $ Mirror tcRaw

          full               = renamed [Replace "full"] $ S.spacing' 0 $ Full

          withGaps           = S.spacing' 0
          variant b v        = renamed [Replace $ b ++ (if v /= "" then " << " ++ v else "")] . reflectVert . deco . reflectVert

