import XMonad

import XMonad.Actions.FocusNth
import XMonad.Actions.CycleWS

import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.BinarySpacePartition

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))

import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- colors

colorForeground = "#c5c5c8"
colorBackground = "#0c0b0b"

colorBlack = "#1a1b1d"
colorRed = "#a03e3e"
colorGreen = "#8c9440"
colorYellow = "#de935f"
colorBlue = "#5f819d"
colorMagenta = "#85678f"
colorCyan = "#55b991"
colorWhite = "#707880"

colorLightBlack = "#202122"
colorLightRed = "#cc6666"
colorLightGreen = "#b5bd68"
colorLightYellow = "#f0c674"
colorLightBlue = "#81a2be"
colorLightMagenta = "#b294bb"
colorLightCyan = "#6cdbb6"
colorLightWhite = "#dbdfdd"

-- settings

myTerm :: String
myTerm = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 2
myModMask = mod4Mask
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
myNormalBorderColor  = colorBackground
myFocusedBorderColor = colorMagenta


-- hooks

myLayoutHook = layoutsWithGaps
  where
    layoutsWithGaps = spacingRaw False (Border 0 0 0 0) True (Border 5 5 5 5) True $ layouts

    layouts = bsp ||| Full
    bsp = reflectVert $ reflectHoriz $ emptyBSP

myManageHook = namedScratchpadManageHook myScratchPads
myEventHook = mempty
myStartupHook = return ()
myLogHook = return ()

-- bindings

myKeys conf@(XConfig {modMask = modm}) = M.fromList $ 
            [ ((modm, xK_Escape), spawn "xmonad --recompile && xmonad --restart")
            , ((modm .|. shiftMask, xK_Escape), io (exitWith ExitSuccess))
            , ((modm .|. shiftMask .|. mod1Mask, xK_Escape), spawn "shutdown -h now")

            -- windows and workspaces
            --, ((modm, 0))
            , ((modm, xK_q), kill)
            , ((modm, xK_space), sendMessage NextLayout)
            , ((modm, xK_j), windows W.focusDown)
            , ((modm, xK_k), windows W.focusUp)
            , ((modm, xK_m ), windows W.focusMaster)
            
            , ((modm, xK_Down), windows W.focusDown)
            , ((modm, xK_Up), windows W.focusUp)
            , ((modm, xK_Left), windows W.focusUp)
            , ((modm, xK_Right), windows W.focusDown)
            
            , ((modm, xK_Return), windows W.swapMaster)
            , ((modm .|. shiftMask, xK_j), windows W.swapDown)
            , ((modm .|. shiftMask, xK_k), windows W.swapUp)

            , ((modm, xK_h), sendMessage Shrink)
            , ((modm, xK_l), sendMessage Expand)

            , ((modm, xK_t), withFocused $ windows . W.sink)
            , ((modm, xK_comma), sendMessage (IncMasterN 1))
            , ((modm, xK_period), sendMessage (IncMasterN (-1)))

            , ((modm, xK_Tab), toggleWS)
            -- , ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
            -- , ((modm .|. controlMask .|. shiftMask, xK_Left), sendMessage $ Move L)
            -- , ((modm .|. controlMask .|. shiftMask, xK_Up), sendMessage $ Move U)
            -- , ((modm .|. controlMask .|. shiftMask, xK_Down), sendMessage $ Move D)

            -- programs
            , ((modm, xK_Return), spawn myTerm)
            , ((modm, xK_space), spawn "rofi -combi-modi run,window -show combi -combi")
            , ((modm .|. controlMask, xK_s), namedScratchpadAction myScratchPads "spotify")
            , ((modm .|. controlMask, xK_m), namedScratchpadAction myScratchPads "slack")
            , ((modm .|. controlMask, xK_d), namedScratchpadAction myScratchPads "discord")
            , ((modm .|. controlMask, xK_v), namedScratchpadAction myScratchPads "netflix")
            , ((modm .|. controlMask, xK_p), namedScratchpadAction myScratchPads "enpass")
            , ((modm .|. controlMask, xK_Return), namedScratchpadAction myScratchPads "terminal")

            -- general
            --, ((controlMask, xK_v), pasteSelection)
            , ((0, xK_F1), spawn "pactl set-sink-mute 0 toggle")
            , ((0, xK_F2), spawn "pactl set-sink-volume 0 -5%")
            , ((0, xK_F3), spawn "pactl set-sink-volume 0 +5%")
            , ((0, xK_F4), spawn "pactl set-source-mute 0 toggle")
            , ((0, xK_F5), spawn "xbacklight -dec 5")
            , ((0, xK_F6), spawn "xbacklight -inc 5")]
            ++
            [((m .|. modm, k), windows $ f i)
                | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


myScratchPads :: [NamedScratchpad]
myScratchPads = []

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh defaults

defaults = def {
    terminal           = myTerm,
    focusFollowsMouse  = myFocusFollowsMouse,
    clickJustFocuses   = myClickJustFocuses,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    keys               = myKeys,

    layoutHook         = myLayoutHook,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    startupHook        = myStartupHook,
    logHook            = myLogHook
  }

help :: String
help = unlines []