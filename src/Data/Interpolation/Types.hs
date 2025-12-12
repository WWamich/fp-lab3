module Interpolation.Types where

type Point = (Double, Double)

data Method = Linear | Newton { order :: Int }
    deriving (Show, Eq)

data Config = Config
    { cMethod :: Method
    , cStep   :: Double 
    } deriving (Show)