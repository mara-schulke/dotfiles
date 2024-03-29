module Mara.Keys where

import XMonad
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (nextScreen, prevScreen, toggleWS)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Hooks.RefocusLast (toggleFocus)
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import Mara.Spacing (border)
import qualified Mara.Theme as Theme
import qualified Mara.Settings as S
import qualified Mara.ScratchPads as SP


modMask :: KeyMask
modMask = mod4Mask

keys :: String -> [([Char], X ())]
keys home =
    [ ("M-C-r",        spawn "xmonad --recompile") -- Recompiles xmonad
    , ("M-S-r",        spawn "xmonad --restart")   -- Restarts xmonad
    , ("M-e e",        io exitSuccess)             -- Quits xmonad

    -- Run Prompt
    , ("M-<Space>",    spawn "rofi -show drun -display-drun \"Run\" -drun-display-format \"{name}\"")
    , ("M-o 1",        spawn "autorandr --load xps")
    , ("M-o 2",        spawn "autorandr --load xps-home")
    , ("M-o 3",        spawn "autorandr --load xps-home-docked")
    , ("M-o 4",        spawn "autorandr --load xps-office")
    , ("M-o 5",        spawn "autorandr --load xps-office-docked")

    -- Useful programs to have a keybinding for launch
    , ("M-<Return>",   spawn S.terminal)
    , ("M-b",          spawn $ S.browser ++ " https://google.com")
    , ("M1-l",         spawn S.lock)

    -- Kill windows
    , ("M-q",          kill1)                  -- Kill the currently focused client
    , ("M-M1-q",       killAll)                -- Kill all windows on current workspace

    -- Workspaces
    , ("M-.",          nextScreen)             -- Switch focus to next monitor
    , ("M-,",          prevScreen)             -- Switch focus to prev monitor

    -- Increase/decrease spacing (gaps)
    , ("M-d",          decScreenSpacing 20)         -- Decrease screen spacing
    , ("M-i",          incScreenSpacing 20)         -- Increase screen spacing
    , ("M-g",          toggleScreenSpacingEnabled <+> (setScreenSpacing $ border (Theme.gaps Theme.theme))) -- Toggle screen spacing
    , ("M-S-g",        toggleScreenSpacingEnabled <+> (setScreenSpacing $ border (Theme.gapsLarge Theme.theme))) -- Toggle screen spacing

    -- Floating Windows into Tiles
    , ("M-t",          withFocused $ windows . W.sink)  -- Push floating window back to tile
    , ("M-M1-t",       sinkAll)                         -- Push ALL floating windows to tile

    -- Windows navigation
    , ("M-f",          sendMessage ToggleLayout >> sendMessage ToggleStruts)       -- Toggle Fullscreen
    , ("M1-<Tab>",     toggleFocus)            -- Toggle the focus between two windows
    , ("M-m",          windows W.focusMaster)  -- Move focus to the master window
    , ("M-S-m",        windows W.swapMaster)   -- Swap the focused window and the master window
    , ("M-S-<Return>", promote)                -- Moves focused window to master, others maintain order
    , ("M-r r",        rotSlavesDown)          -- Rotate all windows except master and keep focus in place
    , ("M-M1-r",       rotAllDown)             -- Rotate all the windows in the current stack

    -- Scratchpads
    , ("M1-t",         namedScratchpadAction SP.scratchPads "shell")        -- Activate the shell scratchpad
    , ("M1-n",         namedScratchpadAction SP.scratchPads "netflix")      -- Activate the netflix scratchpad
    , ("M1-c",         namedScratchpadAction SP.scratchPads "chat")         -- Activate the chat scratchpad
    , ("M1-m",         namedScratchpadAction SP.scratchPads "spotify")      -- Activate the spotify scratchpad
    , ("M1-p",         namedScratchpadAction SP.scratchPads "enpass")       -- Activate the enpass scratchpad
    , ("M1-f",         namedScratchpadAction SP.scratchPads "filemanager")  -- Activate the filemanager scratchpad

    -- Layouts
    , ("M-<Tab>",      toggleWS)
    , ("M-S-<Tab>",    sendMessage NextLayout)
    , ("M-S-<Esc>",    sendMessage FirstLayout)

    -- Multimedia Keys
    -- Unused@T490: XF86Tools, XF86WLAN, XF86Bluetooth, XF86Favorites, XF86Calculator
    , ("<XF86AudioRaiseVolume>",  spawn "vol inc 10")
    , ("<XF86AudioLowerVolume>",  spawn "vol dec 10")
    , ("<XF86AudioMute>",         spawn "vol mute toggle")
    , ("<XF86AudioMicMute>",      spawn "vol mute-mic toggle")
    , ("<XF86MonBrightnessUp>",   spawn "light -A 10")
    , ("<XF86MonBrightnessDown>", spawn "light -U 10")
    , ("<XF86Display>",           spawn "autorandr --change")
    , ("<XF86AudioPlay>",         spawn "playerctl play-pause")
    , ("<XF86AudioNext>",         spawn "playerctl next")
    , ("<XF86AudioPrev>",         spawn "playerctl previous")
    , ("M-<Print>",               spawn "screenshot")
    ]

