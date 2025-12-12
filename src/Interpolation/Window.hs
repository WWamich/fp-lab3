module Interpolation.Window (process) where

import Interpolation.Algo (calcPoint)
import Interpolation.Types

unsafeAt :: [a] -> Int -> a
unsafeAt [] _ = error "выход за границы листа"
unsafeAt (x : _) 0 = x
unsafeAt (_ : xs) n = unsafeAt xs (n - 1)

unsafeLast :: [a] -> a
unsafeLast [] = error "пустой лист"
unsafeLast [x] = x
unsafeLast (_ : xs) = unsafeLast xs

process :: Config -> [Point] -> [Point]
process cfg inputStream = go [] inputStream True
  where
    winSize = case method cfg of
      Linear -> 2
      Newton n -> n

    stepSize = step cfg

    go :: [Point] -> [Point] -> Bool -> [Point]
    go window stream isFirst
      | length window < winSize =
          case stream of
            [] ->
              if length window >= 2
                then finalizeWindow window
                else []
            (p : ps) -> go (window ++ [p]) ps isFirst
      | otherwise =
          let (start, end) = getRange window isFirst
              pointsX = takeWhile (<= end) [start, start + stepSize ..]
              calculated = [(x, calcPoint (method cfg) window x) | x <- pointsX]
              nextWindow = drop 1 window
           in calculated ++ case stream of
                [] -> finalizeWindow window
                (p : ps) -> go (nextWindow ++ [p]) ps False

    getRange :: [Point] -> Bool -> (Double, Double)
    getRange window isFirst
      | isFirst =
          let pStart = window `unsafeAt` 0
              midIdx = length window `div` 2
              pMid = window `unsafeAt` midIdx
           in (fst pStart, fst pMid)
      | otherwise =
          let midIdx = length window `div` 2
              pPrevMid = window `unsafeAt` (midIdx - 1)
              pMid = window `unsafeAt` midIdx
           in (fst pPrevMid + stepSize, fst pMid)

    finalizeWindow :: [Point] -> [Point]
    finalizeWindow window =
      let mid = length window `div` 2
          startCalc = fst (window `unsafeAt` mid) + stepSize

          endCalc = fst (unsafeLast window)

          pts = takeWhile (<= endCalc) [startCalc, startCalc + stepSize ..]
       in [(x, calcPoint (method cfg) window x) | x <- pts]
