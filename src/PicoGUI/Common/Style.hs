{-#LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------------------------------
-- Let's follow CSS / HTML approach - since with all the drawbacks a lot of thought was put in there and we can build upon this foundation if we move towards browser implementation eventually.

-- Some ideas:
--  - Have a GLOBAL styling "sheet" such as css, which we will consult on how to style different "classes"
--  - So, no way around Global state and giving it access to RenderController? Probably ok, since we are not giving access to widgets themselves, but only to the renderer - widgets only say which classes should be used.
--  - Also, let's start calling widgets Components - to follow React
------------------------------------------------------------------------------------------------------

module PicoGUI.Common.Style where

import Data.Text
import NanoVG (V2, V4) -- needs to be changed for standard haskell once hackage is back online :))

data Color = RGBA !Int !Int !Int !Int | RGBAText Text deriving (Show, Eq) -- "ff22cc", to handle html related stuff
data Background = BGColor Color | BGImage Text | BGComplex () deriving (Show, Eq) -- TBD, gradients etc
-- line styles used in borders - "none" will be Nothing
data Stroke = Solid | Dotted | Dashed deriving (Show, Eq)
type Box = V4 Int

-- Dimensions like in CSS more or less:
-- percentage, pixel...
data Dimension = Dpc !Int | Dpx !Int deriving (Show, Eq)

-- need to follow CSS eventually
data Line = Line {
    strokeWidth :: !Dimension,       -- computed width, ALWAYS in pixels
    color       :: Color,
    strokeStyle :: Stroke
} deriving (Show, Eq)

-- this is sort of like css again, but needs to be converted into basic panel with calculated dimensions
data Panel = Panel {
      corner       :: V2 Int -- top left corner, 0 for default
    , singleBorder :: Maybe Line
    , borders      :: V4 (Maybe Line) -- top, right, bottom, left
    , cornerRad    :: !Int -- radius of the rounded corners, if 0 - square
    , background   :: Background
    , padding      :: V4 Int
    , margin       :: V4 Int
    , width        :: Dimension
    , height       :: Dimension
} deriving (Show, Eq)

-- need error handling with fonts!
data Font = Font {
    fontSize  :: !Int,
    fontName  :: Text,
    fontColor :: Color
} deriving (Show, Eq)

-- class similar to CSS class that sets certain properties
data VisualClass = VisualClass {
    name    :: Text -- name of the class, should be unique
  , panel   :: Maybe Panel
  , font    :: Maybe Font
} deriving (Show, Eq)

----------------------------------------------------------------------------------------
-- Tests and stuff
----------------------------------------------------------------------------------------

{-
Based on Darkly bootswatch theme 

$white:    #fff 
$gray-100: #f8f9fa 
$gray-200: #ebebeb 
$gray-300: #dee2e6 
$gray-400: #ced4da 
$gray-500: #adb5bd 
$gray-600: #999 
$gray-700: #444 
$gray-800: #303030 
$gray-900: #222 
$black:    #000 

$blue:    #375a7f 
$indigo:  #6610f2 
$purple:  #6f42c1 
$pink:    #e83e8c 
$red:     #E74C3C 
$orange:  #fd7e14 
$yellow:  #F39C12 
$green:   #00bc8c 
$teal:    #20c997 
$cyan:    #3498DB 

$primary:       $blue 
$secondary:     $gray-700 
$success:       $green 
$info:          $cyan 
$warning:       $yellow 
$danger:        $red 
$light:         $gray-800 
$dark:          $gray-800 
-}

clr_white    = RGBAText "ffffff" 
clr_gray_100 = RGBAText "f8f9fa" 
clr_gray_200 = RGBAText "ebebeb" 
clr_gray_300 = RGBAText "dee2e6" 
clr_gray_400 = RGBAText "ced4da" 
clr_gray_500 = RGBAText "adb5bd" 
clr_gray_600 = RGBAText "9f9f9f" 
clr_gray_700 = RGBAText "4f4f4f" 
clr_gray_800 = RGBAText "303030" 
clr_gray_900 = RGBAText "2f2f2f" 
clr_black    = RGBAText "000000" 

clr_blue   = RGBAText "375a7f" 
clr_indigo = RGBAText "6610f2" 
clr_purple = RGBAText "6f42c1" 
clr_pink   = RGBAText "e83e8c" 
clr_red    = RGBAText "E74C3C" 
clr_orange = RGBAText "fd7e14" 
clr_yellow = RGBAText "F39C12" 
clr_green  = RGBAText "00bc8c" 
clr_teal   = RGBAText "20c997" 
clr_cyan   = RGBAText "3498DB" 

clr_primary       = clr_blue 
clr_secondary     = clr_gray_700 
clr_success       = clr_green 
clr_info          = clr_cyan 
clr_warning       = clr_yellow 
clr_danger        = clr_red 
clr_light         = clr_gray_800 
clr_dark          = clr_gray_800 