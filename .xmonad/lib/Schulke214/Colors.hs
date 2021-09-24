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
foreground = "#ffebe7"

background :: String
background = "#2d262c"

black :: String
black = "#191518"

blackLight :: String
blackLight = "#231d21"

red :: String
red = "#a57e7d"

redLight :: String
redLight = "#f6b0ad"

green :: String
green = "#a59b80"

greenLight :: String
greenLight = "#d3d1a0"

yellow :: String
yellow = "#ab877c"

yellowLight :: String
yellowLight = "#ffc8ae"

blue :: String
blue = "#847aa4"

blueLight :: String
blueLight = "#cbb6e9"

magenta :: String
magenta = "#a882a0"

magentaLight :: String
magentaLight = "#edc3e0"

cyan :: String
cyan = "#7c849f"

cyanLight :: String
cyanLight = "#cfd3fe"

white :: String
white = "#9e8784"

whiteLight :: String
whiteLight = "#ffd6cf"
