{-#LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module PicoGUI.NanoVG.Charts.Chart2D where

import           Foreign.C.Types
import           NanoVG as N hiding (width)
import           Data.Text
import           Data.Vector.Unboxed as U
import           Data.Vector.Generic as G
import           Data.Vector as V

-- these are VISUAL options, which means they should be CALCULATED elsewhere into screen coords
-- this allows for caching, if data changes we recalculate.
-- OR - can we do it via transform matrix??? GPU should be able to handle it?
data AxisOptions = AxisOptions {
    minMax    :: V2 Double -- (min, max) for the axis
  , crossesAt :: !Double -- where it crosses the other axis
  , color     :: Color
} deriving Show

data Chart2DOptions = Chart2DOptions {
    bgColor     :: Color
  , title       :: Text
  , width       :: CFloat
  , height      :: CFloat
  , padding     :: V4 CFloat
  , axisColor   :: Color
  , xAxis       :: AxisOptions
  , yAxis       :: AxisOptions
} deriving Show

defaultChart2DOptions = Chart2DOptions {
    bgColor = rgba 0 0 0 255,
    title = "My Chart",
    width = 1500,
    height = 1000,
    padding = V4 20 20 20 20,
    axisColor = rgba 200 200 200 255,
    xAxis = AxisOptions (V2 (-1) 2.4) 0 (rgba 255 0 0 255),
    yAxis = AxisOptions (V2 0 10  ) 0 (rgba 255 0 0 255)
}

drawBubbles :: Context -> U.Vector (Double, Double, Double) -> V4 CFloat -> IO ()
drawBubbles c dataPoints transVector = do
    putStrLn "Drawing data"
    save c
    let (V4 tx ty sx sy) = transVector
    translate c tx ty
    scale c sx sy
    beginPath c
    fillColor c (rgba 100 200 100 220)
    G.mapM_ drawB dataPoints
    fill c
    restore c
    where drawB (x, y, z) = do
            circle c (realToFrac x) (realToFrac y) (realToFrac z)
            
    

drawChart :: Context -> Chart2DOptions -> CFloat -> CFloat -> IO ()
drawChart c opt x y = do
    save c
    
    translate c x y 
    let w = width opt
        h = height opt
    
    -- chart background
    beginPath c
    rect c 0 0 w h
    fillColor c (bgColor opt)
    fill c

    save c
    let ax =  xAxis opt
        (V2 xmin xmax) = minMax ax
        sc = w / realToFrac (xmax - xmin) -- need to scale the axis
    
    scale c sc 1
    translate c (negate (realToFrac xmin)) 0 -- (negate (realToFrac xmin) * sc) 0
    strokeWidth c (2 / sc)
    strokeColor c (color ax)
    beginPath c
    moveTo c (realToFrac xmin) 0
    lineTo c (realToFrac xmax) 0
    stroke c
    restore c

    -- axis
    let (V4 px1 py1 px2 py2) = padding opt
    strokeWidth c 1
    strokeColor c (axisColor opt)
    beginPath c
    moveTo c px1 (h - py2)
    lineTo c (w - px2) (h - py2)
    moveTo c px1 py1
    lineTo c px1 (h - py1)
    stroke c

    restore c

