module Interpolation.Types
  ( Point,
    Config (..),
    Method (..),
  )
where

type Point = (Double, Double)

data Method = Linear | Newton Int
  deriving (Show, Eq)

data Config = Config
  { method :: Method,
    step :: Double
  }
  deriving (Show, Eq)
