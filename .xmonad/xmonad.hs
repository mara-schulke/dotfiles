-- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, toggleWS)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

-- Data
import Data.Char (isSpace, toUpper)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Text
import Text.Printf

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

-- Custom XMonadTheme Type
data XMonadTheme = XMonadTheme { myForeground :: String
                               , myBackground :: String
                               , myPrimary :: String
                               , mySecondary :: String
                               , myBorderWidth :: Dimension }

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = "code"

myTheme :: XMonadTheme
myTheme = XMonadTheme { myForeground = "#c5c5c8"
                      , myBackground = "#0c0b0b"
                      , myPrimary = "#f0c674"
                      , mySecondary = "#a03e3e"
                      , myBorderWidth = 2 }

altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
          spawnOnce "autorandr --change &"
          spawnOnce "nitrogen --restore &"
          spawnOnce "picom --experimental-backends &"
          -- spawnOnce "nm-applet &"
          -- spawnOnce "volumeicon &"
          -- spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 22 &"
          -- spawnOnce "/usr/bin/emacs --daemon &" -- emacs daemon for the emacsclient
          -- spawnOnce "kak -d -s mysession &"  -- kakoune daemon for better performance
          -- spawnOnce "urxvtd -q -o -f &"      -- urxvt daemon for better performance

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall       = renamed [Replace "tall"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 20
           $ ResizableTall 1 (3/100) (1/2) []
monocle    = renamed [Replace "monocle"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing 20
           $ limitWindows 20 Full
grid       = renamed [Replace "grid"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 20
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals    = renamed [Replace "spirals"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing' 20
           $ spiral (6/7)

myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
               where myDefaultLayout = tall
                                   ||| monocle
                                   ||| grid
                                   ||| spirals

myWorkspaces = ["term", "dev", "firefox", "chat", "tasks", "music", "pw", "sys", "misc"]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ title =? "Mozilla Firefox"                         --> doShift (myWorkspaces !! 2)
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
     , className =? "Enpass"                              --> doShift (myWorkspaces !! 7)
     , isFullscreen                                       --> doFullFloat
     ]

myKeys :: String -> [([Char], X ())]
myKeys home =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile") -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")   -- Restarts xmonad
        , ("M-S-e", io exitSuccess)             -- Quits xmonad

    -- Run Prompt
        -- , ("M-S-<Return>", shellPrompt dtXPConfig) -- Xmonad Shell Prompt
        , ("M-<Space>", spawn "dmenu_run -i -p \"Run: \"") -- Dmenu
        -- , ("M-S-<Return>", spawn "rofi -show drun -config ~/.config/rofi/themes/dt-dmenu.rasi -display-drun \"Run: \" -drun-display-format \"{name}\"") -- Rofi

        -- , ("M-p q", scrotPrompt home True)         -- scrotPrompt True
        -- , ("M-p z", scrotPrompt home False)        -- scrotPrompt False

    -- Useful programs to have a keybinding for launch
        , ("M-<Return>", spawn myTerminal)

    -- Kill windows
        , ("M-q", kill1)     -- Kill the currently focused client
        , ("M-M1-q", killAll)   -- Kill all windows on current workspace

    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor

    -- Increase/decrease spacing (gaps)
        , ("M-d", decWindowSpacing 10)           -- Decrease window spacing
        , ("M-i", incWindowSpacing 10)           -- Increase window spacing
        , ("M-S-d", decScreenSpacing 10)         -- Decrease screen spacing
        , ("M-S-i", incScreenSpacing 10)         -- Increase screen spacing

    -- Windows navigation
        , ("M-m", windows W.focusMaster)        -- Move focus to the master window
        , ("M-S-m", windows W.swapMaster)       -- Swap the focused window and the master window
        , ("M-<Down>", windows W.focusDown)     -- Move focus to the next window
        , ("M-<Up>", windows W.focusUp)         -- Move focus to the prev window
        , ("M-S-<Return>", promote)            -- Moves focused window to master, others maintain order
        , ("M-S-<Down>", windows W.swapDown)         -- Swap focused window with next window
        , ("M-S-<Up>", windows W.swapUp)           -- Swap focused window with prev window
        -- , ("M-S-<Tab>", rotSlavesDown)          -- Rotate all windows except master and keep focus in place
        -- , ("M-C-<Tab>", rotAllDown)             -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-<Tab>", toggleWS)
        , ("M-S-<Tab>", sendMessage NextLayout)
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)  -- Toggles noborder

    -- Increase/decrease windows in the master pane or the stack
        -- , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase number of clients in master pane
        --, ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease number of clients in master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase number of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease number of windows

    -- Window resizing
        , ("M-<Left>", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-<Right>", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-<Left>", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-<Right>", sendMessage MirrorExpand)          -- Exoand vert window width

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        , ("M-C-<Left>", sendMessage $ pullGroup L)
        , ("M-C-<Right>", sendMessage $ pullGroup R)
        , ("M-C-<Up>", sendMessage $ pullGroup U)
        , ("M-C-<Down>", sendMessage $ pullGroup D)
        , ("M-C-m", withFocused (sendMessage . MergeAll))
        , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-C-/", withFocused (sendMessage . UnMergeAll))
        , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
        , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- -- Multimedia Keys
    --     , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
    --     , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
    --     , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))
        , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<XF86Search>", safeSpawn "firefox" ["https://www.duckduckgo.com/"])
        , ("<XF86Mail>", runOrRaise "thunderbird" (resource =? "thunderbird"))
        , ("<XF86Calculator>", runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
        , ("<XF86Eject>", spawn "toggleeject")
        , ("<Print>", spawn "scrotd 0")
        ]

main :: IO ()
main = do
    home <- getHomeDirectory
    xmproc <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
    xmonad $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
        , handleEventHook    = serverModeEventHookCmd <+> serverModeEventHook <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn) <+> docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth myTheme
        , normalBorderColor  = mySecondary myTheme
        , focusedBorderColor = myPrimary myTheme
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = \x -> hPutStrLn xmproc x
            , ppCurrent = xmobarColor (myPrimary myTheme) ""                    -- Current workspace in xmobar
            , ppVisible = xmobarColor (myPrimary myTheme) ""                    -- Visible but not current workspace
            , ppHidden = xmobarColor (mySecondary myTheme) ""                     -- Hidden workspaces in xmobar
            , ppHiddenNoWindows = xmobarColor "#c792ea" ""            -- Hidden workspaces (no windows)
            , ppTitle = xmobarColor (myForeground myTheme) "" . shorten 60         -- Title of active window in xmobar
            , ppSep =  "<fc=" ++ (myBackground myTheme) ++ "> <fn=1>|</fn> </fc>"              -- Separators in xmobar
            , ppUrgent = xmobarColor (myPrimary myTheme) ""                     -- Urgent workspace
            , ppExtras  = [windowCount]                               -- # of windows current workspace
            , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
            }
        } `additionalKeysP` myKeys home
