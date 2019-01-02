module Solution
  ( Solution
  , calculateJobFlows
  , calculateJobsTotalFlow
  ) where

import Job (Job (arrival))
import Assignment (Assignment (finish))

type Solution = [Assignment]

calculateJobFlows :: [Job] -> [Assignment] -> [(Int, Job)]
calculateJobFlows js as =
  let mkFlow acc j =
        let jEnd = maximum [finish a | a <- as]
            jBegin = arrival j
        in acc ++ [(jEnd - jBegin, j)]
  in foldl mkFlow [] js

calculateJobsTotalFlow :: [(Int, Job)] -> Int
calculateJobsTotalFlow jfs = sum $ map fst jfs
