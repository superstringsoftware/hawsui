{-#LANGUAGE OverloadedStrings #-}

module Main where

import SimpleServer

import Network.WebSockets
import Data.Text
import Web.Browser

main :: IO ()
main = runMain "/examples/minimal/ui.html"

