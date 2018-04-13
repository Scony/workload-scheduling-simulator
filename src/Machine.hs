module Machine
  ( Machine
  , ordinaryMachines
  ) where

ordinaryMachines n = [Machine x 1 | x <- [1..n]]

data Machine = Machine { uuid :: Int
                       , capacity :: Int
                       } deriving (Show)
