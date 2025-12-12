module Main where

import Interpolation.Algo (calcPoint)
import Interpolation.Types
import Interpolation.Window (process)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "интерполяция, проверочка"
    [ unitTests,
      propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "Линейная интерпол: чек среднее" $ do
        let pts = [(0, 0), (2, 2)]
        approxEqual (calcPoint Linear pts 1.0) 1.0,
      testCase "Линейная интерпол: чек точек" $ do
        let pts = [(1, 10), (5, 50)]
        approxEqual (calcPoint Linear pts 1.0) 10.0
        approxEqual (calcPoint Linear pts 5.0) 50.0,
      testCase "Ньютон интерпол. при n=3:восстановление y=x^2" $ do
        let pts = [(0, 0), (1, 1), (2, 4)]
        approxEqual (calcPoint (Newton 3) pts 1.5) 2.25,
      testCase "Окно потока.простой линейный поток" $ do
        let cfg = Config {method = Linear, step = 0.5}
        let input = [(0, 0), (1, 1), (2, 2)]
        let result = process cfg input
        let expectedSome = [(0.0, 0.0), (0.5, 0.5), (1.0, 1.0), (1.5, 1.5), (2.0, 2.0)]
        assertBool "Результат совпадает с ожидаемой логикой y=x" $
          all (\(ex, ey) -> any (\(rx, ry) -> rx == ex && abs (ry - ey) < 1e-4) result) expectedSome
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Свойства интерполяции"
    [ testProperty "y=x остается y=x для линейной" prop_linear_identity,
      testProperty "y=x остается y=x для Ньютона" prop_newton_identity
    ]

orderedPointsGen :: Gen [Point]
orderedPointsGen = do
  start <- arbitrary :: Gen Double
  offsets <- listOf1 (choose (0.1, 5.0))
  let xs = scanl (+) start (take 10 offsets)
  return [(x, x) | x <- xs]

prop_linear_identity :: Property
prop_linear_identity =
  forAll orderedPointsGen $ \pts ->
    length pts >= 2 ==>
      let cfg = Config Linear 0.5
          result = process cfg pts
       in all (\(x, y) -> abs (x - y) < 0.001) result

prop_newton_identity :: Property
prop_newton_identity =
  forAll orderedPointsGen $ \pts ->
    length pts >= 3 ==>
      let cfg = Config (Newton 3) 0.5
          result = process cfg pts
       in all (\(x, y) -> abs (x - y) < 0.001) result

approxEqual :: Double -> Double -> Assertion
approxEqual actual expected =
  assertBool
    ("Ожидалось " ++ show expected ++ ", но получили " ++ show actual)
    (abs (actual - expected) < 1e-6)
