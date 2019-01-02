module Job
  ( Job(Job, uuid, arrival)
  ) where

data Job = Job { uuid :: Int
               , priority :: Int
               , arrival :: Int
               } deriving (Show)

instance Read Job where
  readsPrec _ s = do
    let values = map read . words $ s
    [(Job (values !! 0) (values !! 1) (values !! 2), "")]

instance Eq Job where
  Job a _ _ == Job b _ _ = a == b
