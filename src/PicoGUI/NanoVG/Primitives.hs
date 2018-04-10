{-#LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

{-
TODO LIST
- Proper Axis drawing based on chart screen dimensions, axis minMax and where axes cross each other
-}

module PicoGUI.NanoVG.Primitives where

import           Foreign.C.Types
import           NanoVG as N
import           Data.Text
import           PicoGUI.NanoVG.MD.Color

-- import Data.Maybe

-- does nothing when it's Nothing, applies IO action when it's Just
maybe' :: (a -> IO ()) -> Maybe a -> IO ()
maybe' f (Just x) = f x
maybe' f Nothing = return ()

data Panel = Panel {
    dimensions  :: V4 CFloat -- (px,py,w,h) - px,py - coordinates relative to the parent
  , borders     :: V4 (Maybe Border) -- top, right, bottom, left
  , singleBorder :: Maybe Border
  , cornerRad   :: !CFloat -- radius of the rounded corners, if 0 - square
  , background  :: Background
} 

drawPanel :: Context -> Panel -> IO ()
drawPanel c pan = do
    let (V4 px py w h)      = dimensions pan
        (V4 bt br bb bl)    = borders pan
        rad                 = cornerRad pan
        bg                  = background pan
    beginPath c
    fillColor c (solidColor bg)
    maybe' (_drawBorder c) (singleBorder pan)
    roundedRect c px py w h rad 
    fill c
    stroke c
     
 

_drawBorder :: Context -> Border -> IO()
_drawBorder c brd = do
    strokeWidth c (PicoGUI.NanoVG.Primitives.width brd)
    strokeColor c (PicoGUI.NanoVG.Primitives.color (brd :: Border) )
    


testPanel = Panel {
  dimensions = V4 40 600 400 160,
  borders = V4 Nothing Nothing Nothing Nothing,
  cornerRad = 20,
  background = Background Nothing (mdBlue 500),
  singleBorder = Just $ Border 2 (rgba 220 220 100 220) PicoGUI.NanoVG.Primitives.Solid 
}

data Background = Background {
    complexPaint :: Maybe Paint
  , solidColor   :: Color
}

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

-- drawText :: 
drawText c fName size clr x y txt = do
    beginPath c
    fontSize c size
    fontFace c fName
    fillColor c clr
    text c x y txt
    fill c

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
        