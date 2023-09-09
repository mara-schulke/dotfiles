module Mara.Settings where

import XMonad


font :: String
font = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

terminal :: String
terminal = "alacritty"

browser :: String
browser = "chromium"

editor :: String
editor = "vim"

lock :: String
lock = "slock"

workspaces :: [String]
workspaces = ["terminal", "editor", "browser", "database", "ssh", "server", "other", "messages", "config"]

