{-#LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

{-
TODO LIST
- Proper Axis drawing based on chart screen dimensions, axis minMax and where axes cross each other
-}

module PicoGUI.NanoVG.Widgets.Button where

import           Foreign.C.Types
import           NanoVG as N
import           Data.Text
import           PicoGUI.NanoVG.MD.Color
import           PicoGUI.NanoVG.Primitives