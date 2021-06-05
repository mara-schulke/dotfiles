module Schulke214.Colors where


type Color = (Int, Int, Int)

darken :: Float -> Color -> Color
darken f (r, g, b) =
    (applyFactor r, applyFactor g, applyFactor b)
        where limitedFactor = if f < 0 then 0 else if f > 1 then 1 else f
              applyFactor   = darken' limitedFactor

darken' :: Float -> Int -> Int
darken' f c = round $ (fromIntegral c) * f

-- fromHex :: String -> Color
-- toHex :: Color -> String

foreground :: String
foreground = "#c5c5c8"

background :: String
background = "#0c0b0b"

black :: String
black = "#1a1b1d"

blackLight :: String
blackLight = "#202122"

red :: String
red = "#a03e3e"

redLight :: String
redLight = "#cc6666"

green :: String
green = "#8c9440"

greenLight :: String
greenLight = "#b5bd68"

yellow :: String
yellow = "#de935f"

yellowLight :: String
yellowLight = "#f0c674"

blue :: String
blue = "#5e8bb0"

blueLight :: String
blueLight = "#85aed0"

magenta :: String
magenta = "#85678f"

magentaLight :: String
magentaLight = "#b294bb"

cyan :: String
cyan = "#569fb3"

cyanLight :: String
cyanLight = "#79b8df"

white :: String
white = "#707880"

whiteLight :: String
whiteLight = "#dbdfdd"

