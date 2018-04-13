{-#LANGUAGE OverloadedStrings #-}

module PicoGUI.Common.Themes.Test1 where

import PicoGUI.Common.Style as Style
import PicoGUI.Common.Component as Component

import Data.Text
import Linear

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

{-

data Panel = Panel {
      corner       :: V2 Int -- top left corner, 0 for default
    , border :: Maybe Line
    , borders      :: V4 (Maybe Line) -- top, right, bottom, left
    , cornerRad    :: !Int -- radius of the rounded corners, if 0 - square
    , background   :: Background
    , padding      :: V4 Int
    , margin       :: V4 Int
    , width        :: Dimension
    , height       :: Dimension
} deriving (Show, Eq)

data Font = Font {
    fontSize  :: !Int,
    fontName  :: Text,
    fontColor :: Color
} deriving (Show, Eq)

data VisualClass = VisualClass {
    name    :: Text -- name of the class, should be unique
  , panel   :: Maybe Panel
  , font    :: Maybe Font
} deriving (Show, Eq)

-}

defaultFont = Font 16 "sans" clr_black

btnSuccess = VisualClass {
    name = "btnSuccess",
    panel = Just Style.Panel {
        corner       = V2 100 100 -- top left corner, 0 for default
      , border       = Just $ Line (Dpx 2) clr_black Solid
      , borders      = V4 Nothing Nothing Nothing Nothing
      , cornerRad    = 8 -- radius of the rounded corners, if 0 - square
      , background   = BGColor clr_success
      , padding      = V4 8 8 8 8 
      , margin       = V4 0 0 0 0
      , width        = Dpx 0
      , height       = Dpx 0
    },
    font = Just defaultFont
}

btnSuccessHover = btnSuccess {
        name = "btnSuccessHover",
        panel = Just $ ex { background = BGColor clr_cyan }
    } where (Just ex) = panel btnSuccess

testButtonOk = Button { -- not just a button, but any text in a panel - so can be decorated in different ways
    cid                 = "testButtonOk"  
  , labelText           = "OK"
  , defaultButtonStyle  = btnSuccess
  , onHoverButtonStyle  = btnSuccessHover 
}