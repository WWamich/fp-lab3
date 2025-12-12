module Main where

import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Interpolation.Types
import Interpolation.Window (process)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO ()
import Text.Printf (printf)

parseArgs :: [String] -> IO Config
parseArgs args = do
  methodVal <- case getArgValue "--method" args of
    Just "linear" -> return Linear
    Just "newton" -> case getArgValue "--n" args of
      Just n -> return $ Newton (read n)
      Nothing -> die "ошибка --method newton требует --n <int>"
    _ -> die "используй вот это --method <linear|newton> [--n <int>] --step <double>"

  stepVal <- case getArgValue "--step" args of
    Just s -> return (read s)
    Nothing -> die "ошибка --step требуется"

  return $ Config methodVal stepVal

getArgValue :: String -> [String] -> Maybe String
getArgValue _ [] = Nothing
getArgValue flag (x : y : xs)
  | x == flag = Just y
  | otherwise = getArgValue flag (y : xs)
getArgValue _ _ = Nothing

parseLine :: String -> Maybe Point
parseLine s =
  case words (map (\c -> if c == ';' then ' ' else c) s) of
    [x, y] -> Just (read x, read y)
    _ -> Nothing

main :: IO ()
main = do
  args <- getArgs
  config <- parseArgs args
  input <- getContents
  let points = mapMaybe parseLine (lines input)
  let results = process config points
  forM_ results $ uncurry (printf "> %.4f %.4f\n")
