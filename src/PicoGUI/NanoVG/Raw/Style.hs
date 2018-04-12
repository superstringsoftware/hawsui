{-#LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------------------------------
-- "Compiled" representation of the Common style
-- Size and position information goes into low-level Widget, since it's about screen rendering now
-- style class only keeps colors etc
------------------------------------------------------------------------------------------------------

module PicoGUI.NanoVG.Raw.Style where

import           Data.Text
import           Foreign.C.Types
import qualified NanoVG as N
import qualified PicoGUI.Common.Style as Style

data Background = BGColor N.Color | BGComplex N.Paint deriving Show

data Line = Line {
    strokeWidth :: !CFloat,       -- computed width, ALWAYS in pixels
    color       :: N.Color,
    strokeStyle :: Style.Stroke
} deriving (Show, Eq)

data Panel = Panel {
    isComplexBorder :: Bool -- true in case we need to draw borders one by one
  , border          :: Maybe Line -- if complex is false, if nothing - no border
  , borders         :: N.V4 (Maybe Line)
  , cornerRad   :: !CFloat -- radius of the rounded corners, if 0 - square
  , background  :: Background
} deriving Show

data Font = Font {
    fontColor       :: N.Color 
  , fontSize        :: !CFloat
  , fontName        :: Text
} deriving Show

-- class similar to CSS class that sets certain properties
data VisualClass = VisualClass {
    name    :: Text -- name of the class, should be unique
  , font    :: Maybe Font
  , panel   :: Maybe Panel
} deriving Show

