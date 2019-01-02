module Schedule
  ( Schedule
  , calculateSolution
  ) where

import Job (Job (arrival))
import Machine (Machine)
import Operation (parentOf, Operation (duration))
import Assignment (Assignment (Assignment, finish))
import Solution (Solution)

type Schedule = [(Machine, [Operation])]

calculateSolution :: [Job] -> Schedule -> Solution
calculateSolution js s =
  foldl (\a x -> a ++ x) [] $ map (calculateMachineSolution js) s

calculateMachineSolution :: [Job] -> (Machine, [Operation]) -> Solution
calculateMachineSolution js mo =
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
