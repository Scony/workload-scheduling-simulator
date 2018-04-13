module Input
  ( parseInstanceV2
  ) where

import Job
import Operation
import Utils

-- readInstanceV2 = do
--   sequence $ replicate 2 getLine -- omit header

--   rawJobsNum <- getLine
--   let jobsNum = read rawJobsNum :: Int
--   rawJobs <- sequence $ replicate jobsNum getLine
--   let jobs = map read rawJobs :: [Job]

--   rawOperationsNum <- getLine
--   let operationsNum = read rawOperationsNum :: Int
--   rawOperations <- sequence $ replicate operationsNum getLine
--   let operations = map read rawOperations :: [Operation]

--   putStrLn $ show (jobs, operations)

parseInstanceV2 :: [String] -> ([Job], [Operation])
parseInstanceV2 lines =
  let jobsNum = read $ lines !! 2 :: Int
      jobs = map read $ slice 3 (3 + jobsNum) lines :: [Job]
      operationsNum = read $ lines !! (3 + jobsNum) :: Int
      operations = map read
        $ slice (4 + jobsNum) (4 + jobsNum + operationsNum) lines :: [Operation]
  in (jobs, operations)
