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
import Data.List (delete)
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
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
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

-- Custom XMonadTheme Type
data XMonadTheme = XMonadTheme { myForeground :: String
                               , myBackground :: String
                               , myPrimary :: String
                               , mySecondary :: String
                               , myBorderWidth :: Dimension
                               , myGaps :: Integer }

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
myTheme = XMonadTheme { myForeground  = "#c5c5c8"
                      , myBackground  = "#0c0b0b"
                      , myPrimary     = "#5f819d"--"#f0c674"
                      , mySecondary   = "#202122" --"#a03e3e"
                      , myBorderWidth = 2
                      , myGaps        = 20 }

altMask :: KeyMask
altMask = mod1Mask

myStartupHook :: X ()
myStartupHook = do
          spawnOnce "xsetroot -cursor_name left_ptr"
          spawnOnce "autorandr --change && $HOME/.fehbg"
          spawnOnce "picom --experimental-backends &"
          spawnOnce "blueman-applet &"
          spawnOnce "volumeicon &"
          spawnOnce $ "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor primary --transparent true --alpha 0 --tint 0x" ++ (delete '#' $ myBackground myTheme) ++ " --height 16 &"

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing = spacingRaw False (Border i i i i) True (Border i i i i) True
            where i = myGaps myTheme

myTabTheme = def { fontName            = myFont
                 , activeColor         = myPrimary myTheme
                 , inactiveColor       = myBackground myTheme
                 , activeBorderColor   = myPrimary myTheme
                 , inactiveBorderColor = myBackground myTheme
                 , activeTextColor     = myBackground myTheme
                 , inactiveTextColor   = myPrimary myTheme
                 , activeBorderWidth   = myBorderWidth myTheme
                 , inactiveBorderWidth = myBorderWidth myTheme
                 , urgentBorderWidth   = myBorderWidth myTheme
                 }

-- The layout hook
myLayoutHook = avoidStruts
             $ mouseResize
             $ windowArrange
             $ windowNavigation
             $ addTabs shrinkText myTabTheme
             $ toggleLayouts full
             $ lessBorders OnlyScreenFloat
             $ tall ||| full
             where tall = renamed [Replace "tall"] $ mySpacing $ ResizableTall 1 (3/100) (1/2) []
                   full = renamed [Replace "full"] $ noBorders Full

myWorkspaces = ["term", "dev", "firefox", "chat", "tasks", "music", "pw", "sys", "misc"]

centerWindow :: Window -> X ()
centerWindow win = do
    (_, W.RationalRect x y w h) <- floatLocation win
    windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)
    return ()

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ title        =? "Mozilla Firefox"                    --> doShift (myWorkspaces !! 2)
     , (className   =? "firefox" <&&> resource =? "Dialog") --> doFloat
     , className    =? "Code"                               --> doShift (myWorkspaces !! 1)
     , className    =? "discord"                            --> doShift (myWorkspaces !! 3)
     , className    =? "Signal"                             --> doShift (myWorkspaces !! 3)
     , className    =? "Slack"                              --> doShift (myWorkspaces !! 3)
     , className    =? "Enpass"                             --> doShift (myWorkspaces !! 6)
     , isFullscreen                                         --> doFullFloat
     ]

myKeys :: String -> [([Char], X ())]
myKeys home =
    -- Xmonad
        [ ("M-C-r",        spawn "xmonad --recompile") -- Recompiles xmonad
        , ("M-S-r",        spawn "xmonad --restart")   -- Restarts xmonad
        , ("M-e e",        io exitSuccess)             -- Quits xmonad

    -- Run Prompt
        , ("M-<Space>",    spawn "rofi -show drun -display-drun \"Run: \" -drun-display-format \"{name}\"")
        -- , ("M-p q", scrotPrompt home True)         -- scrotPrompt True
        -- , ("M-p z", scrotPrompt home False)        -- scrotPrompt False

    -- Useful programs to have a keybinding for launch
        , ("M-<Return>",   spawn myTerminal)

    -- Kill windows
        , ("M-q",          kill1)                  -- Kill the currently focused client
        , ("M-M1-q",       killAll)                -- Kill all windows on current workspace

    -- Workspaces
        , ("M-.",          nextScreen)             -- Switch focus to next monitor
        , ("M-,",          prevScreen)             -- Switch focus to prev monitor

    -- Increase/decrease spacing (gaps)
        , ("M-d",          decWindowSpacing 10)    -- Decrease window spacing
        , ("M-i",          incWindowSpacing 10)    -- Increase window spacing
        , ("M-S-d",        decScreenSpacing 10)    -- Decrease screen spacing
        , ("M-S-i",        incScreenSpacing 10)    -- Increase screen spacing

    -- Floating Windows into Tiles
        , ("M-t",          withFocused $ windows . W.sink)                             -- Push floating window back to tile
        , ("M-S-t",        sinkAll)                                                    -- Push ALL floating windows to tile

    -- Windows navigation
        , ("M-f",          sendMessage ToggleLayout >> sendMessage ToggleStruts)       -- Toggle Fullscreen without bar
        , ("M-S-f",        sendMessage ToggleLayout)                                   -- Toggle Fullscreen with bar
        , ("M-g",          toggleWindowSpacingEnabled <+> toggleScreenSpacingEnabled)  -- Toggle spacing between windows
        , ("M-m",          windows W.focusMaster)  -- Move focus to the master window
        , ("M-S-m",        windows W.swapMaster)   -- Swap the focused window and the master window
        , ("M-<Left>",     windows W.focusUp)      -- Focus the next window
        , ("M-<Right>",    windows W.focusDown)    -- Focus the previous window
        , ("M-<Up>",       windows W.focusUp)      -- Focus the next window
        , ("M-<Down>",     windows W.focusDown)    -- Focus the previous window
        , ("M-S-<Left>",   windows W.swapUp)       -- Swap the current window with the next window
        , ("M-S-<Right>",  windows W.swapDown)     -- Swap the current window with the previous window
        , ("M-S-<Up>",     windows W.swapUp)       -- Swap the current window with the next window
        , ("M-S-<Down>",   windows W.swapDown)     -- Swap the current window with the previous window
        , ("M-S-<Return>", promote)                -- Moves focused window to master, others maintain order
        , ("M-r r",        rotSlavesDown)          -- Rotate all windows except master and keep focus in place
        , ("M-M1-r",       rotAllDown)             -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-<Tab>",      toggleWS)
        , ("M-S-<Tab>",    sendMessage NextLayout)

    -- -- Window resizing
    --     , ("M-<Left>", sendMessage Shrink)                   -- Shrink horiz window width
    --     , ("M-<Right>", sendMessage Expand)                   -- Expand horiz window width
    --     , ("M-M1-<Left>", sendMessage MirrorShrink)          -- Shrink vert window width
    --     , ("M-M1-<Right>", sendMessage MirrorExpand)          -- Exoand vert window width

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        -- , ("M-C-<Left>", sendMessage $ pullGroup L)
        -- , ("M-C-<Right>", sendMessage $ pullGroup R)
        -- , ("M-C-<Up>", sendMessage $ pullGroup U)
        -- , ("M-C-<Down>", sendMessage $ pullGroup D)
        -- , ("M-C-m", withFocused (sendMessage . MergeAll))
        -- , ("M-C-u", withFocused (sendMessage . UnMerge))
        -- , ("M-C-/", withFocused (sendMessage . UnMergeAll))

    -- -- Multimedia Keys
    --     , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
    --     , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
    --     , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))

        , ("<XF86AudioRaiseVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
        , ("<XF86AudioLowerVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
        , ("<XF86AudioMute>",         spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ("<XF86AudioMicMute>",      spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
        , ("<XF86MonBrightnessUp>",   spawn "light -A 10")
        , ("<XF86MonBrightnessDown>", spawn "light -U 10")
        , ("<XF86Display>",           spawn "autorandr --change")
        , ("<XF86AudioPlay>",         spawn "playerctl play-pause")
        , ("<XF86AudioNext>",         spawn "playerctl next")
        , ("<XF86AudioPrev>",         spawn "playerctl previous")
        , ("<XF86HomePage>",          spawn "firefox")
        , ("<XF86Search>",            safeSpawn "firefox" ["https://www.duckduckgo.com/"])
        , ("<XF86Mail>",              runOrRaise "thunderbird" (resource =? "thunderbird"))
        -- XF86Tools, XF86WLAN, XF86Bluetooth, XF86Favorites
        -- , ("<XF86Calculator>", runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
        , ("M-<Print>",               spawn "scrot")
        ]

main :: IO ()
main = do
    home <- getHomeDirectory
    xmprocPrimary <- spawnPipe "xmobar -x 0"
    xmprocSecondary <- spawnPipe "xmobar -x 1"
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
            { ppOutput = hPutStrLn xmprocPrimary <+> hPutStrLn xmprocSecondary  -- Output pipe
            , ppCurrent = xmobarColor (myPrimary myTheme) ""                    -- Current workspace in xmobar
            , ppVisible = xmobarColor (myPrimary myTheme) ""                    -- Visible but not current workspace
            , ppHidden = xmobarColor (myForeground myTheme) ""                  -- Hidden workspaces in xmobar
            , ppHiddenNoWindows = xmobarColor (myForeground myTheme) ""         -- Hidden workspaces (no windows)
            , ppTitle = xmobarColor (myForeground myTheme) "" . shorten 60      -- Title of active window in xmobar
            , ppSep =  " :: "                                                   -- Separators in xmobar
            , ppUrgent = xmobarColor (myPrimary myTheme) ""                     -- Urgent workspace
            }
        } `additionalKeysP` myKeys home
