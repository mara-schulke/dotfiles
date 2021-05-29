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

myLock :: String
myLock = "slock"

myTheme :: XMonadTheme
myTheme = XMonadTheme { myForeground  = "#c5c5c8"
                      , myBackground  = "#0c0b0b"
                      , myPrimary     = "#85678f"
                      , mySecondary   = "#b294bb"
                      , myBorderWidth = 1
                      , myGaps        = 80 }

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
          spawnOnce $ "trayer --edge bottom --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor primary --transparent true --alpha 0 --tint 0x" ++ (delete '#' $ myBackground myTheme) ++ " --height 16 &"

-- Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing = mySpacing' $ myGaps myTheme

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- The layout hook
myLayoutHook = avoidStruts
             $ mouseResize
             $ windowArrange
             $ windowNavigation
             $ toggleLayouts fullNB
             $ lessBorders OnlyScreenFloat
             $ ifWider 1920 layoutsUHD layoutsHD
             where layoutsUHD   = threecolGaps ||| tallGaps ||| floating ||| full
                   layoutsHD    = threecol ||| tall ||| full
                   threecol     = renamed [Replace "three collumns"] $ mySpacing' 0 $ ThreeColMid 1 (3/100) (1/2) 
                   threecolGaps = renamed [Replace "three collumns"] $ mySpacing $ ThreeColMid 1 (3/100) (1/2) 
                   tall         = renamed [Replace "tall"] $ mySpacing' 0 $ ResizableTall 1 (3/100) (1/2) []
                   tallGaps     = renamed [Replace "tall"] $ mySpacing $ ResizableTall 1 (3/100) (1/2) []
                   floating     = renamed [Replace "floating"] $ simplestFloat
                   full         = renamed [Replace "full"] $ mySpacing' 0 $ Full
                   fullNB       = renamed [Replace "full"] $ mySpacing' 0 $ noBorders $ Full

myWorkspaces = ["sh", "dev", "net", "tasks", "mail", "docs", "misc", "chat", "cfg"]

terminalScratchPad :: String -> String -> NamedScratchpad
terminalScratchPad name cmd = terminalScratchPad' name cmd (0.06, 0.1, 0.88, 0.8)

terminalScratchPad' :: String -> String -> (Rational, Rational, Rational, Rational) -> NamedScratchpad
terminalScratchPad' name cmd (l, t, w, h) = NS name spawnSP findSP manageSP
      where id       = "scratchpad:" ++ name
            spawnSP  = myTerminal ++ " --title " ++ id ++ " -e "  ++ cmd
            findSP   = title =? id
            manageSP = customFloating $ W.RationalRect l t w h

myScratchPads :: [NamedScratchpad]
myScratchPads = [ terminalScratchPad "shell" "zsh"
                , NS "netflix"  spawnNetflix findNetfix manageNetflix
                , NS "spotify" spawnSpotify findSpotify manageSpotify
                , NS "enpass" spawnEnpass findEnpass manageEnpass
                , terminalScratchPad "filemanager" "ranger" ] -- NS "filemanager"  spawnFM findFM manageFM ]
    where spawnNetflix  = "chromium --kiosk --new-window --class=netflix --user-data-dir='/home/max/.config/netflix'"
          findNetfix    = className =? "netflix"
          manageNetflix = customFloating $ W.RationalRect l t w h
                where w = 0.35
                      h = 0.35
                      l = 1 - w
                      t = (1 - h) / 2
          spawnSpotify  = "spotify"
          findSpotify   = className =? "Spotify"
          manageSpotify = customFloating $ W.RationalRect l t w h
                where w = 0.65
                      h = 0.65
                      l = (1 - w) / 2
                      t = (1 - h) / 2
          spawnEnpass   = "Enpass"
          findEnpass    = className =? "Enpass"
          manageEnpass  = customFloating $ W.RationalRect l t w h
                where w = 0.65
                      h = 0.65
                      l = (1 - w) / 2
                      t = (1 - h) / 2
          -- spawnFM       = "nautilus"
          -- findFM        = className =? "Org.gnome.Nautilus"
          -- manageFM      = customFloating $ W.RationalRect l t w h
                -- where w = 0.75
                      -- h = 0.75
                      -- l = (1 - w) / 2
                      -- t = (1 - h) / 2


myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ title        =? "Mozilla Firefox"                    --> doShift (myWorkspaces !! 2)
     , className    =? "Code"                               --> doShift (myWorkspaces !! 1)
     , className    =? "Thunderbird"                        --> doShift (myWorkspaces !! 4)
     , className    =? "discord"                            --> doShift (myWorkspaces !! 7)
     , className    =? "Signal"                             --> doShift (myWorkspaces !! 7)
     , className    =? "Slack"                              --> doShift (myWorkspaces !! 7)
     , className    =? ".blueman-manager-wrapped"           --> doShift (myWorkspaces !! 8)
     , className    =? "Pavucontrol"                        --> doShift (myWorkspaces !! 8)
     , className    =? "Pinentry"                           --> doCenterFloat
     , isDialog                                             --> doCenterFloat
     , isFullscreen                                         --> doFullFloat
     ] <+> namedScratchpadManageHook myScratchPads


myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_CLASS" myManageHook
                      <+> serverModeEventHookCmd
                      <+> serverModeEventHook
                      <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                      <+> docksEventHook
                      <+> handleEventHook def

myKeys :: String -> [([Char], X ())]
myKeys home =
    -- Xmonad
        [ ("M-C-r",        spawn "xmonad --recompile") -- Recompiles xmonad
        , ("M-S-r",        spawn "xmonad --restart")   -- Restarts xmonad
        , ("M-e e",        io exitSuccess)             -- Quits xmonad

    -- Run Prompt
        , ("M-<Space>",    spawn "rofi -show drun -display-drun \"Run\" -drun-display-format \"{name}\"")
        -- , ("M-p q", scrotPrompt home True)         -- scrotPrompt True
        -- , ("M-p z", scrotPrompt home False)        -- scrotPrompt False

    -- Useful programs to have a keybinding for launch
        , ("M-<Return>",   spawn myTerminal)
        , ("M1-l",         spawn myLock)

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
        -- , ("M-<Left>",     windows W.focusUp)      -- Focus the next window
        -- , ("M-<Right>",    windows W.focusDown)    -- Focus the previous window
        -- , ("M-<Up>",       windows W.focusUp)      -- Focus the next window
        -- , ("M-<Down>",     windows W.focusDown)    -- Focus the previous window
        -- , ("M-S-<Left>",   windows W.swapUp)       -- Swap the current window with the next window
        -- , ("M-S-<Right>",  windows W.swapDown)     -- Swap the current window with the previous window
        -- , ("M-S-<Up>",     windows W.swapUp)       -- Swap the current window with the next window
        -- , ("M-S-<Down>",   windows W.swapDown)     -- Swap the current window with the previous window
        , ("M-S-<Return>", promote)                -- Moves focused window to master, others maintain order
        , ("M-r r",        rotSlavesDown)          -- Rotate all windows except master and keep focus in place
        , ("M-M1-r",       rotAllDown)             -- Rotate all the windows in the current stack

    -- Scratchpads
        , ("M1-t",         namedScratchpadAction myScratchPads "shell")        -- Activate the shell scratchpad
        , ("M1-n",         namedScratchpadAction myScratchPads "netflix")      -- Activate the netflix scratchpad
        , ("M1-m",         namedScratchpadAction myScratchPads "spotify")      -- Activate the spotify scratchpad
        , ("M1-p",         namedScratchpadAction myScratchPads "enpass")       -- Activate the enpass scratchpad
        , ("M1-f",         namedScratchpadAction myScratchPads "filemanager")  -- Activate the filemanager scratchpad

    -- Layouts
        , ("M-<Tab>",      toggleWS)
        , ("M-S-<Tab>",    sendMessage NextLayout)

    -- -- Window resizing TODO
    --     , ("M-<Left>", sendMessage Shrink)                   -- Shrink horiz window width
    --     , ("M-<Right>", sendMessage Expand)                   -- Expand horiz window width
    --     , ("M-M1-<Left>", sendMessage MirrorShrink)          -- Shrink vert window width
    --     , ("M-M1-<Right>", sendMessage MirrorExpand)          -- Exoand vert window width

    -- Sublayouts TODO
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        -- , ("M-C-<Left>", sendMessage $ pullGroup L)
        -- , ("M-C-<Right>", sendMessage $ pullGroup R)
        -- , ("M-C-<Up>", sendMessage $ pullGroup U)
        -- , ("M-C-<Down>", sendMessage $ pullGroup D)
        -- , ("M-C-m", withFocused (sendMessage . MergeAll))
        -- , ("M-C-u", withFocused (sendMessage . UnMerge))
        -- , ("M-C-/", withFocused (sendMessage . UnMergeAll))

    -- Multimedia Keys
    -- Unused@T490: XF86Tools, XF86WLAN, XF86Bluetooth, XF86Favorites
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
        , ("<XF86Calculator>",        namedScratchpadAction myScratchPads "python")
        , ("M-<Print>",               spawn "scrot ~/Documents/screenshots/%y%m%d-%H%M%S.png")
        ]

main :: IO ()
main = do
    home <- getHomeDirectory
    xmprocPrimary <- spawnPipe "xmobar -x 0"
    xmprocSecondary <- spawnPipe "xmobar -x 1"
    xmonad $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
        , handleEventHook    = myHandleEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth myTheme
        , normalBorderColor  = mySecondary myTheme
        , focusedBorderColor = myPrimary myTheme
        , logHook = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
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
