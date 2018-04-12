{-#LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

{-
TODO LIST
- Proper Axis drawing based on chart screen dimensions, axis minMax and where axes cross each other
-}

module PicoGUI.NanoVG.Raw.Primitives where

import           Foreign.C.Types
import           NanoVG as N hiding (width)
import           Data.Text
import           PicoGUI.NanoVG.MD.Color
import           PicoGUI.Util

-- import Data.Maybe


data Panel = Panel {
    dimensions  :: V4 CFloat -- (px,py,w,h) - px,py - coordinates relative to the parent
  -- , borders     :: V4 (Maybe Border) -- top, right, bottom, left
  , singleBorder :: Maybe Border
  , cornerRad   :: !CFloat -- radius of the rounded corners, if 0 - square
  , background  :: Background
} deriving Show

-- need error handling with fonts!
data TextLabel = TextLabel {
    labelText :: Text,
    fontSize  :: !CFloat,
    fontName  :: Text,
    fontColor :: Color
} deriving Show

drawPanel :: Context -> Panel -> IO ()
drawPanel c pan = do
    let (V4 px py w h)      = dimensions pan
        --(V4 bt br bb bl)    = borders pan
        rad                 = cornerRad pan
        bg                  = background pan
    save c
    beginPath c
    fillColor c (solidColor bg)
    maybe' (_drawBorder c) (singleBorder pan)
    roundedRect c px py w h rad 
    fill c
    stroke c
    restore c
     
 
_drawBorder :: Context -> Border -> IO()
_drawBorder c brd = do
    strokeWidth c (width brd)
    strokeColor c (PicoGUI.NanoVG.Raw.Primitives.color (brd :: Border) )
    


testPanel = Panel {
  dimensions = V4 40 600 400 160,
  -- borders = V4 Nothing Nothing Nothing Nothing,
  cornerRad = 20,
  background = Background Nothing (mdBlue 500),
  singleBorder = Just $ Border 2 (rgba 220 220 100 220) PicoGUI.NanoVG.Raw.Primitives.Solid 
}

data Background = Background {
    complexPaint :: Maybe Paint
  , solidColor   :: Color
} deriving Show

-- line styles used in borders - "none" will be Nothing
data LineStyle = Solid | Dotted | Dashed deriving (Show, Eq)

data Shadow = Shadow {
    offsetX :: !Int,
    offsetY :: !Int,
    color   :: Color
} deriving (Show, Eq)

-- need to follow CSS eventually
data Border = Border {
    width :: !CFloat,       -- computed width, ALWAYS in pixels
    color :: Color,
    style :: LineStyle
} deriving (Show, Eq)

drawText :: Context -> TextLabel -> IO()
drawText c tl = do
    let fName = fontName tl 
        size  = PicoGUI.NanoVG.Raw.Primitives.fontSize tl
        clr   = fontColor tl
        txt   = labelText tl
    beginPath c
    N.fontSize c size
    fontFace c fName
    fillColor c clr
    text c 0 0 txt
    fill c

drawTextSave c tl = save c >> drawText c tl >> restore c

line :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
line c x1 y1 x2 y2 = do
    beginPath c
    moveTo c x1 y1
    lineTo c x2 y2
    stroke c

lineColor :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> Color -> IO ()
lineColor c x1 y1 x2 y2 col = do
    strokeColor c col
    line c x1 y1 x2 y2
        
-- convert web color format ("#ff23ca") to internal NVG Color
hexToColor :: Text -> Color
hexToColor = undefined