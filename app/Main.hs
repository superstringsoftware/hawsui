{-# LANGUAGE ForeignFunctionInterface #-}
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
import           Graphics.UI.GLFW hiding (Image)
import           NanoVG as NVG
import           NanoVG.Internal.Text as Internal
import           Prelude hiding (init)

import           Foreign.C.Types
import           Foreign.Ptr

import           PicoGUI.NanoVG.Charts.Chart2D


foreign import ccall unsafe "glewInit"
  glewInit :: IO CInt


main :: IO ()
main =
  do e <- init
     unless e $ putStrLn "Failed to init GLFW"
     windowHint $ WindowHint'ContextVersionMajor 3
     windowHint $ WindowHint'ContextVersionMinor 2
     windowHint $ WindowHint'OpenGLForwardCompat True
     windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
     windowHint $ WindowHint'OpenGLDebugContext True
     win <- createWindow 3000 1800 "pico GUI" Nothing Nothing
     case win of
       Nothing -> putStrLn "Failed to create window" >> terminate
       Just w ->
         do makeContextCurrent win
            glewInit
            err1 <- glGetError
            case err1 of
              GL_NO_ERROR -> do 
                putStrLn "All is ok with GL, proceeding..."
                c@(Context c') <- createGL3 (S.fromList [Antialias,StencilStrokes,Debug])
                -- error handling? who needs that anyway
                swapInterval 0
                setTime 0
                whileM_ (not <$> windowShouldClose w) $
                  do  Just t <- getTime
                      (mx,my) <- getCursorPos w
                      (width,height) <- getWindowSize w
                      (fbWidth,fbHeight) <- getFramebufferSize w
                      let pxRatio = fromIntegral fbWidth / fromIntegral width
                      glViewport 0 0 (fromIntegral fbWidth) (fromIntegral fbHeight)
                      glClearColor 0.1 0.1 0.1 1.0
                      glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)
                      beginFrame c (fromIntegral width) (fromIntegral height) pxRatio
                      renderDemo c mx my width height t
                      endFrame c
                      swapBuffers w
                      waitEvents -- pollEvents
              otherwise -> putStrLn ("GL error" ++ show err1) >> terminate

renderDemo :: Context -> Double -> Double -> Int -> Int -> Double -> IO ()
renderDemo c mx my w h t =
  do drawChart c defaultChart2DOptions 150 150
     -- drawBubbles c bubbles (V4 150 1150 10 (-10))
     -- transform matrix to move from (x,y) in top left corner and y-axis down to bottom-left and y-axis up is:
     -- negative yscale (gives reflection) + y translate the size of the height. That's it, very easy.


bubbles :: U.Vector (Double, Double, Double)
bubbles = U.fromList [
        (10,2,3),
        (13,8,2),
        (27,14,2.4)
    ]