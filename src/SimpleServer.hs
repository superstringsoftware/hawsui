{-#LANGUAGE OverloadedStrings #-}

module SimpleServer where

import Network.WebSockets
import Data.Text

simpleServer :: PendingConnection -> IO ()
simpleServer pc = do
    let pr = pendingRequest pc 
    con <- acceptRequest pc
    print $ show pr
    sendTextData con ("Connection received." :: Text)
    let loop = do
            commandMsg <- receiveDataMessage con
            print $ show commandMsg
            sendTextData con ("Hello world!" :: Text)
            loop
    loop 
    