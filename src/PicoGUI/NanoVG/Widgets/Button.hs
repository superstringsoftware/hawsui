{-#LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

{-
Button is a Panel with Text inside + hover / click event handling.
-}

module PicoGUI.NanoVG.Widgets.Button where

import           Foreign.C.Types
import           NanoVG as N
import           Data.Text
import           PicoGUI.NanoVG.MD.Color
import           PicoGUI.NanoVG.Primitives

