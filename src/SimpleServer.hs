{-#LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}

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
    consoleLog con "We received your message, thank you!"
    print $ show obj


data CallFunctionEvent = CallFunctionEvent {
    rootName  :: Text,
    funName   :: [Text],
    argArray  :: Array -- we are using this to call a function on the client side with apply() - so need to pass an Array of args!!!
} deriving (Generic, Show)

instance FromJSON CallFunctionEvent
instance ToJSON CallFunctionEvent where
    toEncoding = genericToEncoding defaultOptions

data CallSingleArgFunctionEvent = CallSingleArgFunctionEvent {
    eventType :: Text,
    rootName  :: Text,
    funName   :: [Text],
    argVal    :: Value
} deriving (Generic, Show)

instance FromJSON CallSingleArgFunctionEvent
instance ToJSON CallSingleArgFunctionEvent where
    toEncoding = genericToEncoding defaultOptions

-- a universal function that calls arbitrary JavaScript function via websocket protocol
callUI :: Connection -> CallFunctionEvent -> IO ()
callUI con ev = sendTextData con (encode ev)

callSingleUI :: Connection -> CallSingleArgFunctionEvent -> IO ()
callSingleUI con ev = sendTextData con (encode ev)


-- helper func
-- we need to change this, as toEncoding is 3x times faster in AESON as they claim!
consoleLog :: Connection -> Text -> IO ()
consoleLog con txt = callSingleUI con ev where
    ev = CallSingleArgFunctionEvent "GlobalFunctionEvent" "window" ["console", "log"] (toJSON txt)