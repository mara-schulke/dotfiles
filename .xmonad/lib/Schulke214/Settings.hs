module Schulke214.Settings where

import XMonad


font :: String
font = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

terminal :: String
terminal = "alacritty"

browser :: String
browser = "surf"

editor :: String
editor = "vim"

lock :: String
lock = "slock"

workspaces :: [String]
workspaces = ["log", "dev", "net", "tasks", "mail", "docs", "misc", "chat", "cfg"]

