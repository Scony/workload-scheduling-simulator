module Operation
  ( Operation(Operation, uuid, duration, parent)
  , parentOf
  ) where

import Job (Job (uuid))

data Operation = Operation { parent :: Int
                           , uuid :: Int
                           , kind :: Int
                           , outcome :: Int
                           , duration :: Int
                           , capacityReq :: Int
                           } deriving (Show)

instance Read Operation where
  readsPrec _ s = [(Operation (fs !! 0) (fs !! 1) (fs !! 2) (fs !! 3) (fs !! 4) (fs !! 5), "")]
    where fs = map read . words $ s

instance Eq Operation where
  Operation _ a _ _ _ _ == Operation _ b _ _ _ _ = a == b

parentOf :: [Job] -> Operation -> Job
parentOf [] op = error "cannot find parent Job"
parentOf (j:js) op
  | Job.uuid j == parent op = j
  | otherwise = parentOf js op
