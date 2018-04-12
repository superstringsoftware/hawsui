
module PicoGUI.NanoVG.Raw.Events where

import           Graphics.UI.GLFW as GLFW
import           Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)

data Event =
      EventError           !GLFW.Error !String
    | EventWindowPos       !GLFW.Window !Int !Int
    | EventWindowSize      !GLFW.Window !Int !Int
    | EventWindowClose     !GLFW.Window
    | EventWindowRefresh   !GLFW.Window
    | EventWindowFocus     !GLFW.Window !GLFW.FocusState
    | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
    | EventFramebufferSize !GLFW.Window !Int !Int
    | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
    | EventCursorPos       !GLFW.Window !Double !Double
    | EventCursorEnter     !GLFW.Window !GLFW.CursorState
    | EventScroll          !GLFW.Window !Double !Double
    | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
    | EventChar            !GLFW.Window !Char
    deriving Show

--------------------------------------------------------------------------------
setupEventChannel win eventsChan = do
    GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan
    GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
    GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
    GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
    GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
    GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
    GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
    GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
    GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
    GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
    GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
    GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
    GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
    GLFW.setCharCallback            win $ Just $ charCallback            eventsChan


-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c