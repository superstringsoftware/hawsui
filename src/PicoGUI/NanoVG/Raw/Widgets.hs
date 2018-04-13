{-#LANGUAGE OverloadedStrings, DuplicateRecordFields, ExistentialQuantification, 
            TypeSynonymInstances, FlexibleInstances, Rank2Types #-}

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
import           PicoGUI.NanoVG.Raw.Events

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

type RawEventHandler datap visual = Event -> RawWidget datap visual -> IO (RawWidget datap visual)
type WEventHandler datap visual  = WEvent -> RawWidget datap visual -> IO (RawWidget datap visual)

-- polymorphic stuff to handle rendering elsewhere via interfaces
-- eventually, need to move this to more efficient structures - vectors or hashmaps
data PolymorphicWidget = forall a. RawWidgetClass a => PW a
type PWList = [PolymorphicWidget]

-- making polymorphic list of widgets an instance of widget itself
instance RawWidgetClass PWList where
    render c = mapM_ (f c)
        where f c (PW x) = render c x

-- Type function generating different widgets with data props of type d and visual props of type v
data RawWidget datap visual = CreateWidget {
    panel       :: Panel, -- handles basic events and background etc.
    dataProps   :: datap,
    visualProps :: visual,
    wid         :: Text, -- unique id of the widget
    renderRW    :: N.Context -> RawWidget datap visual -> IO (),
    -- array of mappings for RAW event handlers: useful for low-level panel manipulation for custom widget development
    revHandlers :: [Event -> RawWidget datap visual -> IO (RawWidget datap visual)],
    -- higher-level event handlers, for taking care of the library users
    evHandlers  :: [WEvent -> RawWidget datap visual -> IO (RawWidget datap visual)] 
}

-- interface to the widgets
class RawWidgetClass w where
    -- rendering a widget in a context
    render :: N.Context -> w -> IO()
    getBoundingBox :: w -> N.V4 CFloat
    isInWidget :: w -> CFloat -> CFloat -> Bool
    getId :: w -> Text
    dispatchWEvent :: w -> WEvent -> IO [w]
    -- addRawEventHandler :: w -> Event -> forall a b. RawEventHandler a b -> w

-- unwrapping polymorphic widget wrapper and making it an instance of the class
instance RawWidgetClass PolymorphicWidget where
    render c (PW w) = render c w
    getBoundingBox (PW w) = getBoundingBox w
    isInWidget (PW w) = isInWidget w
    getId (PW w) = getId w 
    dispatchWEvent (PW w) e = do 
        res <- dispatchWEvent w e
        pure $ Prelude.map PW res
    -- addRawEventHandler (PW w) = PW $ addRawEventHandler w 
    
-- default implementation for RawWidget class
instance RawWidgetClass (RawWidget d v) where
    render c w = renderRW w c w 
    getBoundingBox w = box $ panel w
    getId = wid
    dispatchWEvent = _dispatchWEvent
    isInWidget w x y = let (N.V4 x1 y1 x2 y2) = getBoundingBox w
                       in (x > x1) && (x < x2 + x1) && (y > y1) && (y < y2 + y1)

_dispatchWEvent :: RawWidget datap visual -> WEvent -> IO [(RawWidget datap visual)]
_dispatchWEvent w e = 
    mp' (evHandlers w) e w
    where 
        mp' [] _ _ = pure []
        mp' (f:fs) e' w' = (:) <$> (f e' w') <*> (mp' fs e' w')

-- return list of widgets with coords
inWidget :: PWList -> CFloat -> CFloat -> PWList
inWidget lst x y = Prelude.filter (isIn x y) lst where isIn x y (PW w) = isInWidget w x y

-----------------------------------------------------------------------------------------
-- WIDGETS: Text - basic text label, with or without panel styling
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

addRawEventHandler :: WidgetRawText -> (Event -> WidgetRawText -> IO WidgetRawText) -> WidgetRawText
addRawEventHandler w fh = w { revHandlers = fh: revHandlers w }

addWEventHandler :: WidgetRawText -> (WEvent -> WidgetRawText -> IO WidgetRawText) -> WidgetRawText
addWEventHandler w fh = w { evHandlers = fh: evHandlers w }

standardHandler :: WEvent -> WidgetRawText -> IO WidgetRawText
standardHandler WEventCursorEnter w@(CreateWidget pan dp vp wid _ _ _) = do
    print $ "Handling WEventCursorEnter on " ++ show wid
    pure w

-- handleEvents :: Event -> WidgetRawText -> 
    

-- constructor
createRawText :: Text -> RawTextStyle -> RawTextData -> WidgetRawText
createRawText tid stl txt = CreateWidget {
    panel = createDefaultPanel,
    dataProps = txt,
    visualProps = stl,
    renderRW = _drawText,
    wid = tid,
    evHandlers = [standardHandler]
}

_drawText :: N.Context -> WidgetRawText -> IO ()
_drawText c w = do
    let pan = panel w
        td = dataProps w
        (RawTextStyle (N.V2 x y) fnt shallDrawPanel) = visualProps w
        fName = Style.fontName fnt 
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