module Input
  ( parseInstanceV2
  ) where

import Job (Job)
import Operation (Operation)
import Utils (slice)

parseInstanceV2 :: [String] -> ([Job], [Operation])
parseInstanceV2 lines =
  let jobsNum = read $ lines !! 2 :: Int
      jobs = map read $ slice 3 (3 + jobsNum) lines :: [Job]
      operationsNum = read $ lines !! (3 + jobsNum) :: Int
      operations = map read
        $ slice (4 + jobsNum) (4 + jobsNum + operationsNum) lines :: [Operation]
  in (jobs, operations)
