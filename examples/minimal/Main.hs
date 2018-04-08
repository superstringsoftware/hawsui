{-#LANGUAGE OverloadedStrings #-}

module Main where

import SimpleServer

import Network.WebSockets
import Data.Text
import Web.Browser

import Data.CSV

main :: IO ()
main = runMain "/examples/minimal/ui.html"

