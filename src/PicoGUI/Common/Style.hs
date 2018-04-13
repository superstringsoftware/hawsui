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
import Linear (V2, V4) -- needs to be changed for standard haskell once hackage is back online :))

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
    , border       :: Maybe Line
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
-- Some convenience API
----------------------------------------------------------------------------------------

