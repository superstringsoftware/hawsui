{-#LANGUAGE OverloadedStrings #-}

module Main where

import SimpleServer

import Network.WebSockets
import Data.Text

main :: IO ()
main = runServer "localhost" 8080 simpleServer

    