module Interpolation.Algo (calcPoint) where

import Interpolation.Types

calcPoint :: Method -> [Point] -> Double -> Double
calcPoint Linear points x = linearInterpolation points x
calcPoint (Newton _) points x = newtonInterpolation points x

linearInterpolation :: [Point] -> Double -> Double
linearInterpolation points x =
  case findSegment points x of
    Just ((x0, y0), (x1, y1)) -> y0 + (x - x0) * (y1 - y0) / (x1 - x0)
    Nothing -> 0

findSegment :: [Point] -> Double -> Maybe (Point, Point)
findSegment (p1 : p2 : ps) x
  | x >= fst p1 && x <= fst p2 = Just (p1, p2)
  | otherwise = findSegment (p2 : ps) x
findSegment _ _ = Nothing

newtonInterpolation :: [Point] -> Double -> Double
newtonInterpolation points x =
  let xs = map fst points
      ys = map snd points
      divDiffs = separatedDiffs xs ys
      terms = scanl (\acc xi -> acc * (x - xi)) 1 xs
   in sum $ zipWith (*) divDiffs terms

separatedDiffs :: [Double] -> [Double] -> [Double]
separatedDiffs xs ys =
  let layers = takeWhile (not . null) (iterate calcNextLayer ys)
   in [h | (h : _) <- layers]
  where
    calcNextLayer :: [Double] -> [Double]
    calcNextLayer layer =
      let pairsY = zip layer (drop 1 layer)
          currentShift = length ys - length layer + 1
          pairsX = zip xs (drop currentShift xs)
          inputs = zip pairsY pairsX
       in [ (yNext - yCurr) / (xNext - xCurr)
            | ((yCurr, yNext), (xCurr, xNext)) <- inputs
          ]
