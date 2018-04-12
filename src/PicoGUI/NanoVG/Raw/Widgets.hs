{-#LANGUAGE OverloadedStrings, DuplicateRecordFields, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}

{-
Button is a Panel with Text inside + hover / click event handling.
-}

module PicoGUI.NanoVG.Raw.Widgets where

import           Foreign.C.Types
import qualified NanoVG as N
import           Data.Text
import           PicoGUI.NanoVG.MD.Color
-- import qualified PicoGUI.NanoVG.Raw.Primitives as GUIP
import           Control.Monad (filterM, when)
-- import           PicoGUI.NanoVG.Raw.Events
import qualified PicoGUI.NanoVG.Raw.Style as Style
import           PicoGUI.Util
import           PicoGUI.NanoVG.Raw.Panel

-----------------------------------------------------------------------------------------
{-
Low-Level Widget is:
 - Bounding box (everything is a rectangle for simplicity at first)
 - Visual state (parameters for the render function). In high level we may need several to switch between states quickly based on settings.
 - Data state (also parameters for the render function, but not visual related - e.g., string in a text box, or points data in a chart)
 - Render function that knows how to render the widget based on a visual and data stat
 
 Event handling: the trickiest part. We will probably need to re-fire low-level events from low-level widgets,
 e.g., when a button is clicked - we are catching mousePos and mouseClicked, and then refiring a more high level
 mouse clicked event with more data (like source widget) up the chain?

 So, we can model everything with a Panel and Text then???

 * Handling composite widgets
 Composite widget is basically a root panel with children that are themselves widgets.
 We set them up via polymorphic list Root -> [PL widgets...]
 Then we 'compile' them to a flat representation of the UI - this is needed for FAST access to specific widgets to check where the mouse pointer is etc. Renderer renders this compiled flat Vector of widgets.

 So, a generic widget interface has to support:
 - render
 - getBoundingBox
 - translate (changes top left x,y coordinates - during 'compilation' - to Window coords (bar any additional transforms))
 - handleEvent -- this is still tricky. TBD.

 Then we have: 
  - data-based Tree representation of the UI that we can manipulate (sort of like DOM), recalculated when we resize the window, or add / delete an element etc - and then recompiled into:
  - flat low-level representation of the UI that is rendered every frame
  - we probably need to handle events at both levels??

  Makes sense to start from the intermediate Tree representation API, so that we don't have to build complex interfaces by hand for testing in Raw.
  See PicoGUI.Common?

-}
-----------------------------------------------------------------------------------------

-- Type function generating different widgets with data props of type a and visual props of type b
data RawWidget d v = CreateWidget {
    panel       :: Panel, -- handles basic events and background etc.
    dataProps   :: d,
    visualProps :: v,
    renderRW    :: N.Context -> Panel -> d -> v -> IO ()
}

-- interface to the widgets
class RawWidgetClass w where
    -- rendering a widget in a context
    render :: N.Context -> w -> IO()
    getBoundingBox :: w -> N.V4 CFloat
    isInWidget :: w -> CFloat -> CFloat -> Bool

    
-- default implementation for RawWidget class
instance RawWidgetClass (RawWidget d v) where
    render c w = r c (panel w) (dataProps w) (visualProps w) where r = renderRW w
    getBoundingBox w = box $ panel w
    isInWidget w x y = let (N.V4 x1 y1 x2 y2) = getBoundingBox w
                       in (x > x1) && (x < x2 + x1) && (y > y1) && (y < y2 + y1)


-----------------------------------------------------------------------------------------
-- WIDGETS: Text - basic text label
-----------------------------------------------------------------------------------------

-- visual props data type for the text label: first, top left corner, then visual styling, then whether to draw bounding panel 
data RawTextStyle = RawTextStyle (N.V2 CFloat) Style.Font Bool 
-- data props for the text label
data RawTextData = RawTextData {
    labelText :: Text -- string to display, can be parts of it in text input etc
  , styles    :: [Style.VisualClass] -- global visual class names
}

-- basic Widget-building-block: panel
-- dataProps :: Maybe Text - selector from the GLOBAL class list table, if present
-- visualProps :: RawPanelStyle - data on how to draw
type WidgetRawText = RawWidget RawTextData RawTextStyle

-- constructor
createRawText :: RawTextStyle -> RawTextData -> WidgetRawText
createRawText stl txt = CreateWidget {
    panel = createDefaultPanel,
    dataProps = txt,
    visualProps = stl,
    renderRW = _drawText
}

_drawText :: N.Context -> Panel -> RawTextData -> RawTextStyle -> IO ()
_drawText c pan td (RawTextStyle (N.V2 x y) fnt shallDrawPanel) = do
    let fName = Style.fontName fnt 
        size  = Style.fontSize fnt
        clr   = Style.fontColor fnt
        txt   = labelText td
    when shallDrawPanel (_drawPanel c pan)  
    N.beginPath c
    N.fontSize c size
    N.fontFace c fName -- TODO: ERROR HANDLING!!!!!
    N.fillColor c clr
    N.text c x y txt
    N.fill c

-- drawTextSave c tl = save c >> drawText c tl >> restore c