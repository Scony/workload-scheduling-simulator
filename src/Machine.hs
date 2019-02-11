module Machine
  ( Machine(Machine, uuid, capacity)
  , ordinaryMachines
  ) where

data Machine = Machine { uuid :: Int
                       , capacity :: Int
                       } deriving (Show)

instance Eq Machine where
  Machine a _ == Machine b _ = a == b

ordinaryMachines :: Int -> [Machine]
ordinaryMachines n = [Machine x 1 | x <- [1..n]]
