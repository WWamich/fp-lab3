module Interpolation.Method (calculate) where

import Interpolation.Types

calculate :: Method -> [Point] -> Double -> Double
calculate Linear ps x =  
calculate (Newton n) ps x = 