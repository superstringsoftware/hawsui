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

-- these events are higher level than GLFW we are handling in Raw.Events - they will be dispatched by a low level event processor
-- that will handle stuff like which widget is active etc
data WEvent = 
    WEventCursorEnter -- cursor entered widget area
  | WEventCursorLeave -- cursor left widget area
  deriving Show

type RawEventHandler a b = WEvent -> RawWidget a b -> RawWidget a b


-- Type function generating different widgets with data props of type a and visual props of type b
data RawWidget a b = CreateWidget {
    dataProps   :: a,
    visualProps :: b,
    renderRW    :: N.Context -> a -> b -> IO (),
    -- current approach with event handling - we are simply transforming the current widget Props, nothing else!
    -- theoretically, with composite widgets we then can use "state propagates down" approach like in React
    handleEvents :: [RawEventHandler a b]
}

-- interface to the widgets
class RawWidgetClass w where
    -- rendering a widget in a context
    render :: N.Context -> w -> IO()
    -- rendering a widget with the list of classes to be applied!!! - we don't want the widget's render function to go anywhere global, so passing explicitly!
    -- however, no default implementation
    renderStyled :: N.Context -> w -> [Style.VisualClass] -> IO ()
    handleEvent :: WEvent -> w -> w
    
-- default implementation for render
instance RawWidgetClass (RawWidget a b) where
    render c w = r c (dataProps w) (visualProps w) where r = renderRW w
    -- handleEvent ev w = fold 


-----------------------------------------------------------------------------------------
-- WIDGETS: Panel
-----------------------------------------------------------------------------------------

-- visual props data type for the panel: first, all 4 dimensions (x y w h), then visual styling 
data RawPanelStyle = RawPanelStyle (N.V4 CFloat) Style.Panel

-- basic Widget-building-block: panel
-- dataProps :: Maybe Text - selector from the GLOBAL class list table, if present
-- visualProps :: RawPanelStyle - data on how to draw
type WidgetRawPanel = RawWidget (Maybe Text) RawPanelStyle

-- constructor function
createRawPanel :: RawPanelStyle -> Maybe Text -> [RawEventHandler (Maybe Text) RawPanelStyle] -> WidgetRawPanel 
createRawPanel ps txt handlers = CreateWidget {
    dataProps = txt,
    visualProps = ps,
    renderRW = _drawPanel,
    handleEvents = handlers
}

_drawPanel :: N.Context -> Maybe Text -> RawPanelStyle -> IO ()
_drawPanel c txt (RawPanelStyle (N.V4 px py w h) pan) = do
    let rad = Style.cornerRad pan
        bg  = Style.background pan
    N.save c
    N.beginPath c
    case bg of 
        (Style.BGColor clr) -> N.fillColor c clr
        (Style.BGComplex pnt) -> return()
    maybe' (_drawBorder c) (Style.border pan)
    N.roundedRect c px py w h rad 
    N.fill c
    N.stroke c
    N.restore c
    where _drawBorder c brd = do
            N.strokeWidth c (Style.strokeWidth brd)
            N.strokeColor c (Style.color brd)


-----------------------------------------------------------------------------------------
-- WIDGETS: Text - basic text label
-----------------------------------------------------------------------------------------

-- visual props data type for the text label: first, top left corner, then visual styling 
data RawTextStyle = RawTextStyle (N.V2 CFloat) Style.Font
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
createRawText :: RawTextStyle -> RawTextData -> [RawEventHandler RawTextData RawTextStyle] -> WidgetRawText
createRawText stl txt handlers = CreateWidget {
    dataProps = txt,
    visualProps = stl,
    renderRW = _drawText,
    handleEvents = handlers
}

_drawText :: N.Context -> RawTextData -> RawTextStyle -> IO ()
_drawText c td (RawTextStyle (N.V2 x y) fnt) = do
    let fName = Style.fontName fnt 
        size  = Style.fontSize fnt
        clr   = Style.fontColor fnt
        txt   = labelText td
    N.beginPath c
    N.fontSize c size
    N.fontFace c fName -- TODO: ERROR HANDLING!!!!!
    N.fillColor c clr
    N.text c x y txt
    N.fill c

-- drawTextSave c tl = save c >> drawText c tl >> restore c