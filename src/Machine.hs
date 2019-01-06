module Machine
  ( Machine(Machine)
  , ordinaryMachines
  ) where

ordinaryMachines n = [Machine x 1 | x <- [1..n]]

data Machine = Machine { uuid :: Int
                       , capacity :: Int
                       } deriving (Show)

instance Eq Machine where
  Machine a _ == Machine b _ = a == b
