module Schulke214.Layouts where

import XMonad
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook)
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger (windowArrange)

import qualified Schulke214.Spacing as S


layoutHook =
    avoidStruts
    $ refocusLastLayoutHook
    $ mouseResize
    $ windowArrange
    $ windowNavigation
    $ toggleLayouts (noBorders $ full)
    $ lessBorders OnlyScreenFloat
    $ ifWider 1920 layoutsUHD layoutsHD
    where layoutsUHD   = threecolGaps ||| tallGaps ||| full
          layoutsHD    = threecol ||| tall ||| full
          threecol     = renamed [Replace "three collumns"] $ S.spacing' 0 $ ThreeColMid 1 (3/100) (1/2) 
          threecolGaps = renamed [Replace "three collumns"] $ S.spacing $ ThreeColMid 1 (3/100) (1/2) 
          tall         = renamed [Replace "tall"] $ S.spacing' 0 $ ResizableTall 1 (3/100) (1/2) []
          tallGaps     = renamed [Replace "tall"] $ S.spacing $ ResizableTall 1 (3/100) (1/2) []
          full         = renamed [Replace "full"] $ S.spacing' 0 $ Full

