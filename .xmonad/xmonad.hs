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
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doFullFloat, doCenterFloat, doRectFloat)
import XMonad.Hooks.ServerMode

-- Text
import Text.Printf

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

import qualified Schulke214.Keys as K
import qualified Schulke214.Layouts as L
import qualified Schulke214.ScratchPads as SP
import qualified Schulke214.Settings as S
import qualified Schulke214.Theme as T


myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "autorandr --change && $HOME/.fehbg"
    spawnOnce "picom --experimental-backends &"
    spawnOnce "wpa_gui -t &"
    spawnOnce "blueman-applet &"
    spawnOnce "blueman-manager &"
    spawnOnce "pavucontrol &"
    spawnOnce "volumeicon &"
    spawnOnce $ "trayer --edge bottom --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor primary --transparent true --alpha 0 --tint 0x" ++ (delete '#' $ T.background T.theme) ++ " --height 16 &"

myManageHook :: XMonad.Query (Endo WindowSet)
myManageHook = composeAll
    [ title        =? "Mozilla Firefox"                    --> doShift (S.workspaces !! 2)
    , className    =? "Code"                               --> doShift (S.workspaces !! 1)
    , className    =? "Thunderbird"                        --> doShift (S.workspaces !! 4)
    , className    =? "discord"                            --> doShift (S.workspaces !! 7)
    , className    =? "Signal"                             --> doShift (S.workspaces !! 7)
    , className    =? "Slack"                              --> doShift (S.workspaces !! 7)
    , className    =? ".blueman-manager-wrapped"           --> doShift (S.workspaces !! 8)
    , className    =? "Pavucontrol"                        --> doShift (S.workspaces !! 8)
    , className    =? "Pinentry"                           --> doCenterFloat
    , isDialog                                             --> doCenterFloat
    , isFullscreen                                         --> doFullFloat
    ] <+> namedScratchpadManageHook SP.scratchPads

myHandleEventHook :: Event -> X All
myHandleEventHook =
    dynamicPropertyChange "WM_CLASS" myManageHook
    <+> serverModeEventHookCmd
    <+> serverModeEventHook
    <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
    <+> docksEventHook
    <+> handleEventHook def

myLogHook :: (String -> IO ()) -> X ()
myLogHook output = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
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
    xmonad $ ewmh def
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
        , logHook            = myLogHook $ (hPutStrLn xmprocPrimary <+> hPutStrLn xmprocSecondary)
        } `additionalKeysP` K.keys home
