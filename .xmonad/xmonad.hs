-- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Data
import Data.Monoid (All, Endo)
import Data.List (delete)

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doFullFloat, doCenterFloat, doRectFloat)
import XMonad.Hooks.StatusBar.PP (filterOutWsPP)
import XMonad.Hooks.ServerMode

-- Text
import Text.Printf

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Layout.Spacing (setScreenSpacingEnabled)

import qualified Mara.Keys as K
import qualified Mara.Layouts as L
import qualified Mara.ScratchPads as SP
import qualified Mara.Settings as S
import qualified Mara.Theme as T


myStartupHook :: X ()
myStartupHook = do
    --setScreenSpacingEnabled False
    spawnOnce "source $HOME/.xprofile"
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "autorandr --change && $HOME/.config/feh/fehbg"
    spawnOnce "picom --experimental-backends &"
    spawnOnce "blueman-applet &"
    spawnOnce "blueman-manager &"
    spawnOnce "pavucontrol &"
    spawnOnce $ "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor primary --transparent true --alpha 0 --tint 0x000000 --height 32"

myManageHook :: XMonad.Query (Endo WindowSet)
myManageHook = composeAll
    [ title        =? "Mozilla Firefox"                      --> doShift (S.workspaces !! 2)
    , className    =? "Chromium-browser"                     --> doShift (S.workspaces !! 2)
    , className    =? "discord"                              --> doShift (S.workspaces !! 7)
    , className    =? "Signal"                               --> doShift (S.workspaces !! 7)
    , className    =? "Slack"                                --> doShift (S.workspaces !! 7)
    , className    =? ".blueman-manager-wrapped"             --> doShift (S.workspaces !! 8)
    , className    =? ".blueman-applet-wrapped"              --> doShift (S.workspaces !! 8)
    , className    =? "Pavucontrol"                          --> doShift (S.workspaces !! 8)
    , className    =? "Pinentry"                             --> doCenterFloat
    , isDialog                                               --> doCenterFloat
    , isFullscreen                                           --> doFullFloat
    ] <+> namedScratchpadManageHook SP.scratchPads

myHandleEventHook :: Event -> X All
myHandleEventHook =
    dynamicPropertyChange "WM_CLASS" myManageHook
    <+> serverModeEventHookCmd
    <+> serverModeEventHook
    <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
    <+> handleEventHook def

myLogHook :: (String -> IO ()) -> X ()
myLogHook output = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag]  $ xmobarPP
    { ppOutput          = output                                                -- Output pipe
    , ppCurrent         = xmobarColor (T.primary T.theme) ""                    -- Current workspace in xmobar
    , ppVisible         = xmobarColor (T.primary T.theme) ""                    -- Visible but not current workspace
    , ppHidden          = xmobarColor (T.foreground T.theme) ""                 -- Hidden workspaces in xmobar
    , ppHiddenNoWindows = xmobarColor (T.foreground T.theme) ""                 -- Hidden workspaces (no windows)
    , ppTitle           = xmobarColor (T.foreground T.theme) "" . shorten 59    -- Title of active window in xmobar
    , ppSep             =  " :: "                                               -- Separators in xmobar
    , ppUrgent          = xmobarColor (T.primary T.theme) ""                    -- Urgent workspace
    }

main :: IO ()
main = do
    home <- getHomeDirectory
    xmprocPrimary <- spawnPipe "xmobar -x 0"
    xmprocSecondary <- spawnPipe "xmobar -x 1"
    xmprocTertiary <- spawnPipe "xmobar -x 2"
    xmonad $ docks $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
        , handleEventHook    = myHandleEventHook
        , startupHook        = myStartupHook
        , modMask            = K.modMask
        , terminal           = S.terminal
        , workspaces         = S.workspaces
        , layoutHook         = L.layoutHook
        , borderWidth        = T.border T.theme
        , normalBorderColor  = T.secondary T.theme
        , focusedBorderColor = T.primary T.theme
        , logHook            = myLogHook $ (hPutStrLn xmprocPrimary <+> hPutStrLn xmprocSecondary <+> hPutStrLn xmprocTertiary)
        } `additionalKeysP` K.keys home `removeKeysP` ["M-S-q"]
