{-#LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module PicoGUI.NanoVG.Raw.Panel where

import           Foreign.C.Types
import qualified NanoVG as N
import           Data.Text
import           PicoGUI.NanoVG.MD.Color
-- import qualified PicoGUI.NanoVG.Raw.Primitives as GUIP
import           Control.Monad (filterM)
-- import           PicoGUI.NanoVG.Raw.Events
import qualified PicoGUI.NanoVG.Raw.Style as Style
import           PicoGUI.Util

-----------------------------------------------------------------------------------------
{-
Low-Level Widget is:
 - Bounding box (everything is a rectangle for simplicity at first)
 - Visual state (parameters for the render function). In high level we may need several to switch between states quickly based on settings.
 - Data state (also parameters for the render function, but not visual related - e.g., string in a text box, or points data in a chart)
 - Render function that knows how to render the widget based on a visual and data stat
 
What if we try a different approach - every widget is a Panel, which handles basic visuals (border, background etc) as well as Event Dispatch?
Then we take a render function and pass it a partial function that takes some additional parameters for displaying stuff?

-}
-----------------------------------------------------------------------------------------

-- these events are higher level than GLFW we are handling in Raw.Events - they will be dispatched by a low level event processor
-- that will handle stuff like which widget is active etc
data WEvent = 
    WEventCursorEnter -- cursor entered widget area
  | WEventCursorLeave -- cursor left widget area
  deriving Show

type PanelEventHandler = WEvent -> Panel -> Panel -- ok, we need a state monad here, no way around it. Events need to flow.

data Panel = CreatePanel {
    style   :: Style.Panel
  , box     :: N.V4 CFloat
  , id      :: Text
  , handler :: PanelEventHandler
}

_drawPanel :: N.Context -> Panel -> IO ()
_drawPanel c pan = do
    let (N.V4 px py w h) = box pan
        rad = Style.cornerRad $ style pan
        bg  = Style.background $ style pan
    N.save c
    N.beginPath c
    case bg of 
        (Style.BGColor clr) -> N.fillColor c clr
        (Style.BGComplex pnt) -> return()
    maybe' (_drawBorder c) (Style.border $ style pan)
    N.roundedRect c px py w h rad 
    N.fill c
    N.stroke c
    N.restore c
    where _drawBorder c brd = do
            N.strokeWidth c (Style.strokeWidth brd)
            N.strokeColor c (Style.color brd)


