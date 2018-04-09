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
    vec     :: U.Vector Float -- vector of one data point, of whatever dimensionality
  , label   :: Text
} -- deriving Show

-- how we store data points in a series
data SeriesData = Series1D (U.Vector Float) | SeriesFull (V.Vector DataPoint)
-- how a series should be displayed (or the whole chart)
data SeriesType = PointS | ScatterS | BubbleS | BarS | LineS

data DataSeries = DataSeries {
    points  :: SeriesData
  , title   :: Text
  , color   :: Color
  , stype   :: SeriesType
} -- deriving Show

-- what if we have columns? like a spreadsheet? TBD.

testSeries1 = DataSeries {
    points = Series1D $ U.fromList [1.0,2.3,5.1,2,3,4,5,4,3,5],
    title = "Series 1",
    color = rgb 255 100 100,
    stype = LineS
}

testSeries2 = DataSeries {
    points = Series1D $ U.fromList [2,1,3,4,7,3,9,3,2.4,5.1],
    title = "Series 2",
    color = rgb 100 100 255,
    stype = LineS
}
