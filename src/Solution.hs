module Solution
  ( Solution
  , calculateJobFlows
  , calculateJobsTotalFlow
  ) where

import Job (Job (arrival))
import Assignment (Assignment (finish, operation))
import Operation (parentOf)

type Solution = [Assignment]

calculateJobFlows :: [Job] -> [Assignment] -> [(Int, Job)]
calculateJobFlows js as =
  let mkFlow acc j = acc ++ [(jEnd - jBegin, j)]
        where
          jEnd = maximum [finish a | a <- as, j == parentOf js (operation a)]
          jBegin = arrival j
  in foldl mkFlow [] js

calculateJobsTotalFlow :: [(Int, Job)] -> Int
calculateJobsTotalFlow jfs = sum $ map fst jfs
