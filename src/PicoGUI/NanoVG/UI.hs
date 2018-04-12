{-#LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

{-
Button is a Panel with Text inside + hover / click event handling.
-}

module PicoGUI.NanoVG.UI where

--------------------------------------------------------------------------------

import           Foreign.C.Types
import           NanoVG as N
import           PicoGUI.NanoVG.MD.Color

import qualified Data.Set as S

import           Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import           Control.Monad             (unless, when, void)
import           Control.Monad.Trans.RWS.Strict  (RWST, ask, asks, evalRWST, get, modify, put)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.List                 (intercalate)
import           Data.Maybe                (catMaybes)
import           Text.PrettyPrint as PP

-- import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLFW          as GLFW
import           PicoGUI.NanoVG.Widgets.Widget

import           PicoGUI.NanoVG.Raw.Events

--------------------------------------------------------------------------------

data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    , envContext       :: !N.Context
    }

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateMouseDown       :: !Bool
    , stateDragging        :: !Bool
    , stateDragStartX      :: !Double
    , stateDragStartY      :: !Double
  --  , stateUIWidgets       :: !PWList
  --  , stateCursorInW       :: !PWList
    }

type Demo = RWST Env () State IO

--------------------------------------------------------------------------------
-- can we get out of the 1-function GLEW dependency???

foreign import ccall unsafe "glewInit"
  glewInit :: IO CInt
--------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

-- here we modified it to run on functions that take both a window (for base opengl) and NVG Context.

withWindow :: Int -> Int -> String -> (GLFW.Window -> N.Context -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        windowHint $ WindowHint'ContextVersionMajor 3
        windowHint $ WindowHint'ContextVersionMinor 2
        windowHint $ WindowHint'OpenGLForwardCompat True
        windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
        windowHint $ WindowHint'OpenGLDebugContext True
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just w) -> do
                GLFW.makeContextCurrent m
                glewInit
                c@(Context c') <- N.createGL3 (S.fromList [Antialias,StencilStrokes,Debug])
                f w c
                GLFW.setErrorCallback $ Just simpleErrorCallback
                GLFW.destroyWindow w
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------

processEvents :: Demo ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
      Just e -> do
          processEvent e
          processEvents
      Nothing -> return ()

processEvent :: Event -> Demo ()
processEvent ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          win <- asks envWindow
          liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowPos _ x y) ->
          printEvent "window pos" [show x, show y]

      (EventWindowSize _ width height) ->
          printEvent "window size" [show width, show height]

      (EventWindowClose _) ->
          printEvent "window close" []

      (EventWindowRefresh _) ->
          printEvent "window refresh" []

      (EventWindowFocus _ fs) ->
          printEvent "window focus" [show fs]

      (EventWindowIconify _ is) ->
          printEvent "window iconify" [show is]

      (EventFramebufferSize _ width height) -> do
          printEvent "framebuffer size" [show width, show height]
          modify $ \s -> s
            { stateWindowWidth  = width
            , stateWindowHeight = height
            }
          -- adjustWindow

      (EventMouseButton _ mb mbs mk) -> do
          printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
          when (mb == GLFW.MouseButton'1) $ do
              let pressed = mbs == GLFW.MouseButtonState'Pressed
              modify $ \s -> s
                { stateMouseDown = pressed
                }
              unless pressed $
                modify $ \s -> s
                  { stateDragging = False
                  }

      (EventCursorPos _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "cursor pos" [show x', show y']
          state <- get
          c <- asks envContext
          -- let ui = stateUIWidgets state
          -- inw <- liftIO $ inWidget c ui (realToFrac x) (realToFrac y)
          -- mapM_ (liftIO . print . show) inw
          {-
          put $ state
              { stateCursorInW       = inw
              }
          -}
          when (stateMouseDown state && not (stateDragging state)) $
            put $ state
              { stateDragging        = True
              , stateDragStartX      = x
              , stateDragStartY      = y
              -- , stateCursorInW       = inw
              }

      (EventCursorEnter _ cs) ->
          printEvent "cursor enter" [show cs]

      (EventScroll _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "scroll" [show x', show y']
          -- env <- ask
          {-
          modify $ \s -> s
            { stateZDist =
                let zDist' = stateZDist s + realToFrac (negate $ y / 2)
                in curb (envZDistClosest env) (envZDistFarthest env) zDist'
            }
          -}
          -- adjustWindow

      (EventKey win k scancode ks mk) -> do
          printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
          when (ks == GLFW.KeyState'Pressed) $ do
              -- Q, Esc: exit
              when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
                liftIO $ GLFW.setWindowShouldClose win True
              -- ?: print instructions
              when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
                liftIO printInstructions
              -- i: print GLFW information
              when (k == GLFW.Key'I) $
                liftIO $ printInformation win

      (EventChar _ c) ->
          printEvent "char" [show c]



getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
    x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up
    x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down
    y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left
    y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right
    let x0n = if x0 then (-1) else 0
        x1n = if x1 then   1  else 0
        y0n = if y0 then (-1) else 0
        y1n = if y1 then   1  else 0
    return (x0n + x1n, y0n + y1n)

getJoystickDirections :: GLFW.Joystick -> IO (Double, Double)
getJoystickDirections js = do
    maxes <- GLFW.getJoystickAxes js
    return $ case maxes of
      (Just (x:y:_)) -> (-y, x)
      _              -> ( 0, 0)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

--------------------------------------------------------------------------------

printInstructions :: IO ()
printInstructions =
    putStrLn $ PP.render $
      nest 4 (
        PP.text "------------------------------------------------------------" $+$
        PP.text "'?': Print these instructions"                                $+$
        PP.text "'i': Print GLFW information"                                  $+$
        PP.text ""                                                             $+$
        PP.text "* Mouse cursor, keyboard cursor keys, and/or joystick"        $+$
        PP.text "  control rotation."                                          $+$
        PP.text "* Mouse scroll wheel controls distance from scene."           $+$
        PP.text "------------------------------------------------------------"
      )

printInformation :: GLFW.Window -> IO ()
printInformation win = do
    version       <- GLFW.getVersion
    versionString <- GLFW.getVersionString
    monitorInfos  <- runMaybeT getMonitorInfos
    joystickNames <- getJoystickNames
    clientAPI     <- GLFW.getWindowClientAPI              win
    cv0           <- GLFW.getWindowContextVersionMajor    win
    cv1           <- GLFW.getWindowContextVersionMinor    win
    cv2           <- GLFW.getWindowContextVersionRevision win
    robustness    <- GLFW.getWindowContextRobustness      win
    forwardCompat <- GLFW.getWindowOpenGLForwardCompat    win
    debug         <- GLFW.getWindowOpenGLDebugContext     win
    profile       <- GLFW.getWindowOpenGLProfile          win

    putStrLn $ PP.render $
      nest 4 (
        PP.text "------------------------------------------------------------" $+$
        PP.text "GLFW C library:" $+$
        nest 4 (
          PP.text "Version:"        <+> renderVersion version $+$
          PP.text "Version string:" <+> renderVersionString versionString
        ) $+$
        PP.text "Monitors:" $+$
        nest 4 (
          renderMonitorInfos monitorInfos
        ) $+$
        PP.text "Joysticks:" $+$
        nest 4 (
          renderJoystickNames joystickNames
        ) $+$
        PP.text "OpenGL context:" $+$
        nest 4 (
          PP.text "Client API:"            <+> renderClientAPI clientAPI $+$
          PP.text "Version:"               <+> renderContextVersion cv0 cv1 cv2 $+$
          PP.text "Robustness:"            <+> renderContextRobustness robustness $+$
          PP.text "Forward compatibility:" <+> renderForwardCompat forwardCompat $+$
          PP.text "Debug:"                 <+> renderDebug debug $+$
          PP.text "Profile:"               <+> renderProfile profile
        ) $+$
        PP.text "------------------------------------------------------------"
      )
  where
    renderVersion (GLFW.Version v0 v1 v2) =
        PP.text $ intercalate "." $ map show [v0, v1, v2]

    renderVersionString =
        PP.text . show

    renderMonitorInfos =
        maybe (PP.text "(error)") (vcat . map renderMonitorInfo)

    renderMonitorInfo (name, (x,y), (w,h), vms) =
        PP.text (show name) $+$
        nest 4 (
          location <+> size $+$
          fsep (map renderVideoMode vms)
        )
      where
        location = int x <> PP.text "," <> int y
        size     = int w <> PP.text "x" <> int h <> PP.text "mm"

    renderVideoMode (GLFW.VideoMode w h r g b rr) =
        brackets $ res <+> rgb <+> hz
      where
        res = int w <> PP.text "x" <> int h
        rgb = int r <> PP.text "x" <> int g <> PP.text "x" <> int b
        hz  = int rr <> PP.text "Hz"

    renderJoystickNames pairs =
        vcat $ map (\(js, name) -> PP.text (show js) <+> PP.text (show name)) pairs

    renderContextVersion v0 v1 v2 =
        hcat [int v0, PP.text ".", int v1, PP.text ".", int v2]

    renderClientAPI         = PP.text . show
    renderContextRobustness = PP.text . show
    renderForwardCompat     = PP.text . show
    renderDebug             = PP.text . show
    renderProfile           = PP.text . show

type MonitorInfo = (String, (Int,Int), (Int,Int), [GLFW.VideoMode])

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos =
    getMonitors >>= mapM getMonitorInfo
  where
    getMonitors :: MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors

    getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
    getMonitorInfo mon = do
        name <- getMonitorName mon
        vms  <- getVideoModes mon
        MaybeT $ do
            pos  <- liftIO $ GLFW.getMonitorPos mon
            size <- liftIO $ GLFW.getMonitorPhysicalSize mon
            return $ Just (name, pos, size, vms)

    getMonitorName :: GLFW.Monitor -> MaybeT IO String
    getMonitorName mon = MaybeT $ GLFW.getMonitorName mon

    getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon

getJoystickNames :: IO [(GLFW.Joystick, String)]
getJoystickNames =
    catMaybes `fmap` mapM getJoystick joysticks
  where
    getJoystick js =
        fmap (maybe Nothing (\name -> Just (js, name)))
             (GLFW.getJoystickName js)

--------------------------------------------------------------------------------

printEvent :: String -> [String] -> Demo ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

curb :: Ord a => a -> a -> a -> a
curb l h x
  | x < l     = l
  | x > h     = h
  | otherwise = x

--------------------------------------------------------------------------------

joysticks :: [GLFW.Joystick]
joysticks =
  [ GLFW.Joystick'1
  , GLFW.Joystick'2
  , GLFW.Joystick'3
  , GLFW.Joystick'4
  , GLFW.Joystick'5
  , GLFW.Joystick'6
  , GLFW.Joystick'7
  , GLFW.Joystick'8
  , GLFW.Joystick'9
  , GLFW.Joystick'10
  , GLFW.Joystick'11
  , GLFW.Joystick'12
  , GLFW.Joystick'13
  , GLFW.Joystick'14
  , GLFW.Joystick'15
  , GLFW.Joystick'16
  ]
