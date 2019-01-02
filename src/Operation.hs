module Operation
  ( Operation(Operation, uuid, duration)
  , parentOf
  ) where

import Job (Job (uuid))

parentOf :: [Job] -> Operation -> Job -- TODO maybe ?
parentOf js op = [j | j <- js, Job.uuid j == Operation.parent op] !! 0

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
