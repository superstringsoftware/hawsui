{-#LANGUAGE OverloadedStrings, DeriveGeneric #-}

module SimpleServer where

import Network.WebSockets
import Data.Text
import Web.Browser
import System.Directory
import Data.Aeson

import GHC.Generics

-- very general event, simply passing event data as they come
-- in real apps, it's best to provide specific types
data GenericEvent = GenericEvent {
    eventName :: Text,
    eventData :: Value
} deriving (Generic, Show)


instance ToJSON GenericEvent where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON GenericEvent


runMain ui = do
    cd <- getCurrentDirectory
    let path = "file://" ++ cd ++ ui
    print $ "trying to open " ++ path
    res <- openBrowser path 
    if not res then print "Couldn't open UI html as per path above - please try opening manually" >> runServer "localhost" 8080 simpleServer
    else runServer "localhost" 8080 simpleServer



simpleServer :: PendingConnection -> IO ()
simpleServer pc = do
    let pr = pendingRequest pc 
    con <- acceptRequest pc
    print $ show pr
    sendTextData con ("Connection received." :: Text)
    let loop = do
            commandMsg <- receiveDataMessage con
            processClientEvent con commandMsg
            loop
    loop 
    
-- processing events coming from the client
-- it has to be composed eventually
processClientEvent :: Connection -> DataMessage -> IO ()
processClientEvent con (Binary _) = print "Binary message processing not implemented"

processClientEvent con (Text msg _) = do
    print $ show msg
    let obj = decode msg :: Maybe GenericEvent
    sendTextData con ("Accepted" :: Text)
    print $ show obj