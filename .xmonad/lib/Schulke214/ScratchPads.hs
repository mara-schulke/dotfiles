module Schulke214.ScratchPads where

-- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (nextScreen, prevScreen, toggleWS)
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)

-- Data
import Data.Monoid
import Data.Tree
import Data.List (delete)

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doFullFloat, doCenterFloat, doRectFloat)
import XMonad.Hooks.ServerMode

-- Layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimplestFloat

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

-- Text
import Text.Printf

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

import qualified Schulke214.Settings as S


termPad :: String -> String -> Query (Endo WindowSet) -> NamedScratchpad
termPad name cmd manageSP = NS name spawnSP findSP manageSP
    where id       = "scratchpad:" ++ name
          spawnSP  = S.terminal ++ " --title " ++ id ++ " -e "  ++ cmd
          findSP   = title =? id


scratchPads :: [NamedScratchpad]
scratchPads =
    [ termPad           "shell"      "zsh"       (doRectFloat 0.9)
    , NS "netflix"      spawnNetflix findNetfix  manageNetflix
    , NS "chat"         spawnChat    findChat    manageChat
    , NS "spotify"      spawnSpotify findSpotify manageSpotify
    , NS "enpass"       spawnEnpass  findEnpass  manageEnpass
    , NS "filemanager"  spawnFM      findFM      manageFM
    ]
    where spawnNetflix  = "chromium --kiosk --new-window --class=netflix --user-data-dir='/home/max/.config/netflix'"
          findNetfix    = className =? "netflix"
          manageNetflix = doRectFloat 1

          spawnChat     = "chromium --kiosk --new-window --class=chat --user-data-dir='/home/max/.config/chat'"
          findChat      = className =? "chat"
          manageChat    = doRectFloat 0.8

          spawnSpotify  = "spotify"
          findSpotify   = className =? "Spotify"
          manageSpotify = doRectFloat 0.8

          spawnEnpass   = "Enpass"
          findEnpass    = className =? "Enpass"
          manageEnpass  = doRectFloat 0.8

          spawnFM       = "nautilus"
          findFM        = className =? "Org.gnome.Nautilus"
          manageFM      = doRectFloat 0.8

          doRectFloat r = customFloating $ W.RationalRect ((1 - r) / 2) ((1 - r) / 2 * (16 / 9)) r (r / (16 / 9) * (r * 1.85))
