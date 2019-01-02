module Solution
  ( calculateAssignments
  , calculateJobFlows
  , calculateJobsTotalFlow
  ) where

import Job (Job (arrival))
import Machine (Machine)
import Operation (parentOf, Operation (duration))
import Assignment (Assignment (Assignment, finish))

calculateAssignments :: [Job] -> [(Machine, [Operation])] -> [Assignment]
calculateAssignments js mos =
  foldl (\a x -> a ++ x) [] $ map (calculateMachineAssignments js) mos

calculateMachineAssignments :: [Job] -> (Machine, [Operation]) -> [Assignment]
calculateMachineAssignments js mo =
  let m = fst mo
      ops = snd mo
      mkAssignment [] op =
        let jobArrival = arrival $ parentOf js op
        in [Assignment (jobArrival + duration op) op m]
      mkAssignment acc op =
        let finishOfPrev = finish $ last acc
            jobArrival = arrival $ parentOf js op
            beginTime = max finishOfPrev jobArrival
        in acc ++ [Assignment (beginTime + duration op) op m]
  in foldl mkAssignment [] ops

calculateJobFlows :: [Job] -> [Assignment] -> [(Int, Job)]
calculateJobFlows js as =
  let mkFlow acc j =
        let jEnd = maximum [finish a | a <- as]
            jBegin = arrival j
        in acc ++ [(jEnd - jBegin, j)]
  in foldl mkFlow [] js

calculateJobsTotalFlow :: [(Int, Job)] -> Int
calculateJobsTotalFlow jfs = sum $ map fst jfs
