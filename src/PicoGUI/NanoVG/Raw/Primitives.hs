{-#LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

{-
TODO LIST
- Proper Axis drawing based on chart screen dimensions, axis minMax and where axes cross each other
-}

module PicoGUI.NanoVG.Raw.Primitives where

import           Foreign.C.Types
import           NanoVG as N hiding (width)
import           PicoGUI.NanoVG.MD.Color

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
        