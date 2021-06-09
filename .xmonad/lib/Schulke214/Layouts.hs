module Schulke214.Layouts where

import XMonad
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook)
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


dynGaps m l = (m $ S.spacing' 0 $ l, m $ S.spacing $ l)

layoutHook =
    avoidStruts
    $ refocusLastLayoutHook
    $ mouseResize
    $ windowArrange
    $ windowNavigation
    $ toggleLayouts (noBorders $ full)
    $ lessBorders OnlyScreenFloat
    $ ifWider 2560 layoutsUHD layoutsHD
    where layoutsUHD               = tallGaps ||| threecolGaps ||| full
          layoutsHD                = tall     ||| threecol     ||| full
          (threecol, threecolGaps) = dynGaps (renamed [Replace "three collumns"] . reflectHoriz) $ ThreeColMid 1 (3/100) (1/2)
          (tall    , tallGaps)     = dynGaps (renamed [Replace "tall"]) $ ResizableTall 1 (3/100) (1/2) []
          full                     = renamed [Replace "full"] $ S.spacing' 0 $ Full

