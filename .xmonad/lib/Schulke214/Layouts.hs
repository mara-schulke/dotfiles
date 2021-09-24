module Schulke214.Layouts where

import XMonad
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook)
import XMonad.Layout
import XMonad.Layout.LayoutModifier
import XMonad.Layout.ResizableTile
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
    where layoutsUHD           = tallGaps ||| tallGapsMirrored ||| threecolGaps ||| threecolGapsMirrored ||| full
          layoutsHD            = tall     ||| tallMirrored     ||| threecol     ||| threecolMirrored     ||| full

          threecolMod variant  = renameByVariant "three collumns" variant . reflectVert . deco . reflectVert
          threecolRaw          = ThreeColMid 1 (3/100) (1/2)
          threecol             = threecolMod "" $ noGaps   threecolRaw
          threecolGaps         = threecolMod "" $ withGaps threecolRaw
          threecolMirrored     = threecolMod "mirror" $ noGaps   $ Mirror threecolRaw
          threecolGapsMirrored = threecolMod "mirror" $ withGaps $ Mirror threecolRaw

          tallMod variant      = renameByVariant "tall" variant . reflectVert . deco . reflectVert
          tallRaw              = reflectVert $ ResizableTall 1 (3/100) (1/2) []
          tall                 = tallMod "" $ noGaps   $ tallRaw
          tallGaps             = tallMod "" $ withGaps $ tallRaw
          tallMirrored         = tallMod "mirror" $ noGaps   $ Mirror tallRaw
          tallGapsMirrored     = tallMod "mirror" $ withGaps $ Mirror tallRaw

          full                 = renamed [Replace "full"] $ noGaps $ Full

          noGaps               = S.spacing' 0
          withGaps             = S.spacing
          renameByVariant b v  = renamed [Replace $ b ++ (if v /= "" then " << " ++ v else "")]

