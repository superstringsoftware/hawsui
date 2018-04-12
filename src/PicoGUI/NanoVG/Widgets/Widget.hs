{-#LANGUAGE OverloadedStrings, DuplicateRecordFields, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}

{-
Button is a Panel with Text inside + hover / click event handling.
-}

module PicoGUI.NanoVG.Widgets.Widget where

import           Foreign.C.Types
import           NanoVG as N
import           Data.Text
import           PicoGUI.NanoVG.MD.Color
import           PicoGUI.NanoVG.Raw.Primitives as GUIP
import           Control.Monad (filterM)

{-
-- interface for the widgets
class NVGWidget a where
    recalculate    :: a -> a
    render         :: Context -> a -> IO ()
    getBoundingBox :: Context -> a -> IO BoundingBox

-- polymorphic list of widgets
data PolymorphicWidget = forall a. (NVGWidget a, Show a) => PW a
type PWList = [PolymorphicWidget]

instance Show PolymorphicWidget where
    show (PW w) = show w

addToUI :: (NVGWidget a, Show a) => a -> PWList -> PWList
addToUI w ui = PW w : ui

-- making polymorphic list of widgets an instance of widget itself
instance NVGWidget PWList where
    recalculate p = p
    render c = mapM_ (f c)
        where f c (PW x) = render c x

-- making a basic panel a widget
instance NVGWidget Panel where
    recalculate p = p
    render = drawPanel
    getBoundingBox c p = pure $ dimensions p

-- panel with children - generic basis for custom widgets, a panel with a bunch of widgets rendered relative to it inside
data PanelwChildren = PanelwChildren {
    rootPanel :: Panel
  , children  :: PWList
} deriving Show

instance NVGWidget PanelwChildren where 
    recalculate p = p
    getBoundingBox c p = pure $ dimensions $ rootPanel p
    render c p = do 
        let pn = rootPanel p
            (V4 x y w h) = dimensions pn
        save c
        render c pn
        translate c x y -- children need to be drawn relative to the root panel
        scissor c x y w h -- also need to handle scisoring properly
        mapM_ (f c) (children p)
        restore c
        where f c (PW x) = render c x
         

instance NVGWidget TextLabel where
    recalculate p = p
    getBoundingBox c tl = do
        (Bounds bb) <- textBounds c 0 0 (labelText tl)
        return bb
    render = drawText

-- first custom composite widget - panel and a text label, turning into a button
-- actually, input text box with one line will be exactly the same, only with somewhat different state handling (gutter position and visible part of text)
data Button = Button {
    panel :: Panel,
    textLabel :: TextLabel,
    padding :: V4 CFloat,
    isDirty :: Bool -- flag to be set when panel size needs recalculation - e.g., when we are changing text or font size etc 
} deriving Show

instance NVGWidget Button where
    recalculate p = p
    getBoundingBox c bt = pure $ dimensions $ panel bt
    render c bt = do
        let pan = panel bt
            tl  = textLabel bt
            (V4 x1 y1 x2 y2) = padding bt
            (V4 px1 py1 _ _) = dimensions pan
        save c
        render c pan
        translate c (px1 + x1) (py1 + y1)
        render c tl
        restore c

data InputText = InputText {
    button :: Button,
    cursorPos :: !Int, -- which letter of text string is the cursor on
    displayedStringCoords :: V2 Int
} deriving Show

instance NVGWidget InputText where
    recalculate p = p
    getBoundingBox c it = pure $ dimensions $ panel $ button it
    render c it = do
        let pan = panel $ button it
            tl  = textLabel $ button it
            (V4 x1 y1 x2 y2) = padding $ button it
            (V4 px1 py1 _ _) = dimensions pan
        save c
        render c pan
        translate c (px1 + x1) (py1 + y1)
        render c tl
        restore c

-- returns a widget in which current coordinates are
inWidget :: Context -> PWList -> CFloat -> CFloat -> IO PWList
inWidget c lst x y = filterM (isIn c x y) lst
    where isIn c x y (PW w) = do
            b@(V4 x1 y1 x2 y2) <- getBoundingBox c w
            -- putStrLn $ "Checking if " ++ show x ++ ", " ++ show y ++ " is in " ++ show b
            return $ (x > x1) && (x < x2 + x1) && (y > y1) && (y < y2 + y1) 

-}