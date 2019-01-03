module Solution
  ( Solution
  , calculateJobFlows
  , calculateJobsTotalFlow
  , costs
  ) where

import Job (Job (arrival, uuid))
import Assignment (Assignment (finish, operation))
import Operation (parent)

type Solution = [Assignment]

calculateJobFlows :: [Job] -> [Assignment] -> [(Int, Job)]
calculateJobFlows js as = costs js as flow

calculateJobsTotalFlow :: [(Int, Job)] -> Int
calculateJobsTotalFlow jfs = sum $ map fst jfs

-- totalFlow :: (Num a) => [Job] -> [Assignment] -> a
-- totalFlow js as = total $ costs js as flow

costs :: [Job] -> [Assignment] -> (Job -> [Assignment] -> (a, Job)) -> [(a, Job)]
costs js as cost = map (\x -> cost x as) js

total :: (Num a) => [(a, Job)] -> a
total cjs = sum $ map fst cjs

flow :: Job -> [Assignment] -> (Int, Job)
flow j as = (jEnd - jBegin, j)
  where
    jEnd = maximum [finish a | a <- as, uuid j == (parent . operation) a]
    jBegin = arrival j
