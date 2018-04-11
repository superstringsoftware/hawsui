{-#LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

{-
Handling low level events from glfw and dispatching them to listeners
-}

module PicoGUI.NanoVG.EventProcessor where

import           Foreign.C.Types
import           NanoVG as N
import           Data.Text
import           Data.Vector
--import           Data.List


type BoundingBox = V4 CFloat
type WidgetContainer = []

{-
class EventData a where
    processEvent :: a -> IO b
-}

data EventfulWidget = EventfulWidget {
    bbox   :: BoundingBox,
    id     :: !Int,
    textId :: Text
}

data UIRepresentation = UIRepresentation {
    widgets :: WidgetContainer EventfulWidget,
    isDirty :: Bool
}

addWidgetToUI :: UIRepresentation -> EventfulWidget -> UIRepresentation
addWidgetToUI urep ew = urep { widgets = neww }
    where neww = ew:(widgets urep)


{-
-- returns a widget in which current coordinates are
inWidget :: UIRepresentation -> CFloat -> CFloat -> [EventfulWidget]
inWidget urep x y = Prelude.filter (isIn x y) (widgets urep) 
    where isIn x y w = 
            let (V4 x1 y1 x2 y2) = bbox w
            in (x > x1) && (x < x2) && (y > y1) && (y < y2) 
-}