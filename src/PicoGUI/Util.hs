module PicoGUI.Util where

-- does nothing when it's Nothing, applies IO action when it's Just
maybe' :: (a -> IO ()) -> Maybe a -> IO ()
maybe' f (Just x) = f x
maybe' f Nothing = return ()