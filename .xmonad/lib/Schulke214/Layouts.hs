module Schulke214.Layouts where

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

import qualified Schulke214.Spacing as S
import qualified Schulke214.Theme as T

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
    $ ifWider 2560 layoutsUHD layoutsHD
    where layoutsUHD         = tallGaps ||| tallMirrorGaps ||| gridGaps ||| tcGaps ||| tcMirrorGaps ||| full
          layoutsHD          = tall     ||| tallMirror     ||| grid     ||| tc     ||| tcMirror     ||| full

          tallRaw            = reflectVert $ ResizableTall 1 (3/100) (1/2) []
          tall               = variant "tall" "" $ noGaps tallRaw
          tallGaps           = variant "tall" "" $ withGaps tallRaw
          tallMirror         = variant "tall" "mirror" $ noGaps $ Mirror tallRaw
          tallMirrorGaps     = variant "tall" "mirror" $ withGaps $ Mirror tallRaw

          gridRaw            = GridRatio (4/3)
          grid               = variant "grid" "" $ noGaps gridRaw
          gridGaps           = variant "grid" "" $ withGaps gridRaw

          tcRaw              = ThreeColMid 1 (3/100) (1/2)
          tc                 = variant "threecol" "" $ noGaps tcRaw
          tcGaps             = variant "threecol" "" $ withGaps tcRaw
          tcMirror           = variant "threecol" "mirror" $ noGaps $ Mirror tcRaw
          tcMirrorGaps       = variant "threecol" "mirror" $ withGaps $ Mirror tcRaw

          full               = renamed [Replace "full"] $ noGaps $ Full

          noGaps             = S.spacing' 0
          withGaps           = S.spacing
          variant b v        = renamed [Replace $ b ++ (if v /= "" then " << " ++ v else "")] . reflectVert . deco . reflectVert

