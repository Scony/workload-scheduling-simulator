module Input
  ( parseInstanceV2
  ) where

import Job (Job)
import Operation (Operation)
import Utils (slice)

parseInstanceV2 :: [String] -> ([Job], [Operation])
parseInstanceV2 lns =
  let jobsNum = read $ lns !! 2 :: Int
      jobs = map read $ slice 3 (3 + jobsNum) lns :: [Job]
      operationsNum = read $ lns !! (3 + jobsNum) :: Int
      operations = map read
        $ slice (4 + jobsNum) (4 + jobsNum + operationsNum) lns :: [Operation]
  in (jobs, operations)
