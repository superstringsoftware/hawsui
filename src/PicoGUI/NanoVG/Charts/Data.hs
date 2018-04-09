{-#LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

-- Data types for Charts. Might eventually move to Common?

module PicoGUI.NanoVG.Charts.Data where

import           Foreign.C.Types
import           Data.Text
import           Data.Vector.Unboxed as U
import           Data.Vector.Generic as G
import           Data.Vector as V
import           NanoVG as N

data DataPoint = DataPoint {
    vec     :: U.Vector CFloat -- vector of one data point, of whatever dimensionality
  , label   :: Text
} -- deriving Show

data SeriesType = Series1D (U.Vector CFloat) | SeriesFull (V.Vector DataPoint)

data DataSeries = DataSeries {
    points  :: SeriesType
  , title   :: Text
  , color   :: Color
} -- deriving Show

-- what if we have columns? like a spreadsheet? TBD.

