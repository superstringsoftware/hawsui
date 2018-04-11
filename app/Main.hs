{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative (pure)
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans.Maybe
import           Data.Bits hiding (rotate)
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Graphics.GL.Core32
import           Graphics.UI.GLFW as GLFW hiding (Image)
import           NanoVG as NVG
import           NanoVG.Internal.Text as Internal
import           Prelude hiding (init)

import           Foreign.C.Types
import           Foreign.Ptr

import           PicoGUI.NanoVG.Charts.Chart2D
import           PicoGUI.NanoVG.Primitives
import           PicoGUI.NanoVG.MD.Color
import           PicoGUI.NanoVG.Widgets.Widget as W
import           PicoGUI.NanoVG.UI

import           Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import           Control.Monad.Trans.RWS.Strict  (RWST, ask, asks, evalRWST, get, modify, put, gets)
import           Control.Monad.IO.Class (liftIO)

main = mainCycle

mainCycle :: IO ()
mainCycle = do
    let width  = 2000
        height = 1200

    eventsChan <- newTQueueIO :: IO (TQueue Event)

    withWindow width height "Pico UI" $ \win c -> do
        setupEventChannel win eventsChan

        defaultFont <- NVG.createFont c "sans" (FileName "nanovg/example/Roboto-Regular.ttf")
        -- error handling? who needs that anyway
        GLFW.swapInterval 0
        GLFW.setTime 0

        (fbWidth, fbHeight) <- GLFW.getFramebufferSize win

        let zDistClosest  = 10
            zDistFarthest = zDistClosest + 20
            zDist         = zDistClosest + ((zDistFarthest - zDistClosest) / 2)
            env = Env { 
              envEventsChan    = eventsChan
            , envWindow        = win
            , envContext       = c
            }
            state = State { 
              stateWindowWidth     = fbWidth
            , stateWindowHeight    = fbHeight
            , stateMouseDown       = False
            , stateDragging        = False
            , stateDragStartX      = 0
            , stateDragStartY      = 0
            , stateUIWidgets       = testUI
            , stateCursorInW       = []
            }
        runDemo env state

    putStrLn "ended!"


runDemo :: Env -> State -> IO ()
runDemo env state = do
    printInstructions
    void $ evalRWST run env state

run :: Demo ()
run = do
    w  <- asks envWindow
    c  <- asks envContext
    ui <- gets stateUIWidgets

    (mx,my) <- liftIO $ getCursorPos w
    -- putStrLn $ "Cursor position: " ++ (show mx) ++ ", " ++ (show my)
    (width,height) <- liftIO $ getWindowSize w
    (fbWidth,fbHeight) <- liftIO $ getFramebufferSize w
    let pxRatio = fromIntegral fbWidth / fromIntegral width
    liftIO $ glViewport 0 0 (fromIntegral fbWidth) (fromIntegral fbHeight)
    liftIO $ glClearColor 0.1 0.1 0.1 1.0
    liftIO $ glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)
    liftIO $ beginFrame c (fromIntegral width) (fromIntegral height) pxRatio
    liftIO $ NVG.textAlign c (S.fromList [AlignTop])
    -- liftIO $ renderDemo c mx my width height
    liftIO $ render c ui
    liftIO $ endFrame c
    liftIO $ swapBuffers w
    liftIO $ waitEvents -- pollEvents

    processEvents

    q <- liftIO $ GLFW.windowShouldClose w
    unless q run

renderDemo :: Context -> Double -> Double -> Int -> Int -> IO ()
renderDemo c mx my w h =
  do drawChart c defaultChart2DOptions 150 150
     drawPanel c testPanel
     drawTestText c
     translate c 1000 60
     -- drawBubbles c bubbles (V4 150 1150 10 (-10))
     -- transform matrix to move from (x,y) in top left corner and y-axis down to bottom-left and y-axis up is:
     -- negative yscale (gives reflection) + y translate the size of the height. That's it, very easy.

drawTestText c = do
    save c
    translate c 400 200
    drawText c "sans" 36 (mdWhite) "Hello World"
    translate c 0 100
    drawText c "sans" 100 (mdWhite) "Heading"
    restore c

bubbles :: U.Vector (Double, Double, Double)
bubbles = U.fromList [
        (10,2,3),
        (13,8,2),
        (27,14,2.4)
    ]

testButton = Button {
    panel = Panel {
        dimensions = V4 600 200 120 60,
        cornerRad = 8,
        background = Background Nothing (mdGrey 300),
        singleBorder = Just $ Border 2 (rgba 220 220 100 220) PicoGUI.NanoVG.Primitives.Solid 
    },
    textLabel = TextLabel {
        W.text = "Enter",
        W.fontSize = 48,
        fontName = "sans",
        fontColor = mdBlack
    },
    W.padding = V4 8 8 8 8,
    isDirty = False
}

testInput = InputText {
    button =  Button {
        panel = Panel {
            dimensions = V4 600 300 220 60,
            cornerRad = 0,
            background = Background Nothing mdWhite,
            singleBorder = Just $ Border 2 (rgb 220 50 220) PicoGUI.NanoVG.Primitives.Solid 
        },
        textLabel = TextLabel {
            W.text = "input",
            W.fontSize = 48,
            fontName = "sans",
            fontColor = mdBlack
        },
        W.padding = V4 8 8 8 8,
        isDirty = False
    },
    cursorPos = 0,
    displayedStringCoords = V2 0 0
}

testUI :: PWList
testUI = [PW testButton, PW testInput]