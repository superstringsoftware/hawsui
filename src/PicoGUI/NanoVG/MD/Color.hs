{-# LANGUAGE OverloadedStrings #-}
module PicoGUI.NanoVG.MD.Color where

import           Data.Word
import           NanoVG (Color, rgba)

-- Materal design colors implemented as pattern-matched functions

mdBlack = rgba 0 0 0 255
mdWhite = rgba 255 255 255 255

mdGrey :: Int -> Color
mdGrey 500 = rgba 0x9E 0x9E 0x9E 255
mdGrey 50  = rgba 0xFA 0xFA 0xFA 255
mdGrey 100 = rgba 0xF5 0xF5 0xF5 255
mdGrey 300 = rgba 0xE0 0xE0 0xE0 255
mdGrey 700 = rgba 0x61 0x61 0x61 255
mdGrey 900 = rgba 0x21 0x21 0x21 255
mdGrey _   = rgba 0x9E 0x9E 0x9E 255

mdBlue :: Int -> Color
mdBlue 500 = rgba 0x21 0x96 0xF3 255
mdBlue _   = rgba 0x21 0x96 0xF3 255

mdRed :: Int -> Color
mdRed 500  = rgba 0xF4 0x43 0x36 255
mdRed  _   = rgba 0xF4 0x43 0x36 255
