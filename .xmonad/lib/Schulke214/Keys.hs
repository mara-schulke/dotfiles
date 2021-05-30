module Schulke214.Keys where

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
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import qualified Schulke214.Settings as S
import qualified Schulke214.ScratchPads as SP


modMask :: KeyMask
modMask = mod4Mask

keys :: String -> [([Char], X ())]
keys home =
    [ ("M-C-r",        spawn "xmonad --recompile") -- Recompiles xmonad
    , ("M-S-r",        spawn "xmonad --restart")   -- Restarts xmonad
    , ("M-e e",        io exitSuccess)             -- Quits xmonad

    -- Run Prompt
    , ("M-<Space>",    spawn "rofi -show drun -display-drun \"Run\" -drun-display-format \"{name}\"")
    -- , ("M-p q", scrotPrompt home True)         -- scrotPrompt True
    -- , ("M-p z", scrotPrompt home False)        -- scrotPrompt False

    -- Useful programs to have a keybinding for launch
    , ("M-<Return>",   spawn S.terminal)
    , ("M1-l",         spawn S.lock)

    -- Kill windows
    , ("M-q",          kill1)                  -- Kill the currently focused client
    , ("M-M1-q",       killAll)                -- Kill all windows on current workspace

    -- Workspaces
    , ("M-.",          nextScreen)             -- Switch focus to next monitor
    , ("M-,",          prevScreen)             -- Switch focus to prev monitor

    -- Increase/decrease spacing (gaps)
    , ("M-d",          decWindowSpacing 20 <+> decScreenSpacing 20)    -- Decrease window spacing
    , ("M-i",          incWindowSpacing 20 <+> incScreenSpacing 20)    -- Increase window spacing

    -- Floating Windows into Tiles
    , ("M-t",          withFocused $ windows . W.sink)  -- Push floating window back to tile
    , ("M-M1-t",       sinkAll)                         -- Push ALL floating windows to tile

    -- Windows navigation
    , ("M-f",          sendMessage ToggleLayout >> sendMessage ToggleStruts)       -- Toggle Fullscreen
    , ("M-g",          toggleWindowSpacingEnabled <+> toggleScreenSpacingEnabled)  -- Toggle spacing between windows
    , ("M-m",          windows W.focusMaster)  -- Move focus to the master window
    , ("M-S-m",        windows W.swapMaster)   -- Swap the focused window and the master window
    , ("M-S-<Return>", promote)                -- Moves focused window to master, others maintain order
    , ("M-r r",        rotSlavesDown)          -- Rotate all windows except master and keep focus in place
    , ("M-M1-r",       rotAllDown)             -- Rotate all the windows in the current stack

    -- Scratchpads
    , ("M1-t",         namedScratchpadAction SP.scratchPads "shell")        -- Activate the shell scratchpad
    , ("M1-n",         namedScratchpadAction SP.scratchPads "netflix")      -- Activate the netflix scratchpad
    , ("M1-m",         namedScratchpadAction SP.scratchPads "spotify")      -- Activate the spotify scratchpad
    , ("M1-p",         namedScratchpadAction SP.scratchPads "enpass")       -- Activate the enpass scratchpad
    , ("M1-f",         namedScratchpadAction SP.scratchPads "filemanager")  -- Activate the filemanager scratchpad

    -- Layouts
    , ("M-<Tab>",      toggleWS)
    , ("M-S-<Tab>",    sendMessage NextLayout)

    -- Multimedia Keys
    -- Unused@T490: XF86Tools, XF86WLAN, XF86Bluetooth, XF86Favorites, XF86Calculator
    , ("<XF86AudioRaiseVolume>",  spawn "vol inc 10")
    , ("<XF86AudioLowerVolume>",  spawn "vol dec 10")
    , ("<XF86AudioMute>",         spawn "vol mute toggle")
    , ("<XF86AudioMicMute>",      spawn "vol mute-mic toggle")
    , ("<XF86MonBrightnessUp>",   spawn "light -A 2.5")
    , ("<XF86MonBrightnessDown>", spawn "light -U 2.5")
    , ("<XF86Display>",           spawn "autorandr --change")
    , ("<XF86AudioPlay>",         spawn "playerctl play-pause")
    , ("<XF86AudioNext>",         spawn "playerctl next")
    , ("<XF86AudioPrev>",         spawn "playerctl previous")
    , ("<XF86HomePage>",          spawn "firefox")
    , ("<XF86Search>",            safeSpawn "firefox" ["https://www.duckduckgo.com/"])
    , ("<XF86Mail>",              runOrRaise "thunderbird" (resource =? "thunderbird"))
    , ("M-<Print>",               spawn "scrot ~/Documents/screenshots/%y%m%d-%H%M%S.png")
    ]

