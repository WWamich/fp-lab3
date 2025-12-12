module Interpolation.Window (streamProcess) where

import Interpolation.Types
import Interpolation.Method

streamProcess :: Config -> [Point] -> [Point]
streamProcess cfg inputPoints =  
