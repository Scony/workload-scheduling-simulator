module Schedule
  ( Schedule
  , calculateSolution
  ) where

import Job (Job (arrival))
import Machine (Machine)
import Operation (parentOf, Operation (duration))
import Assignment (Assignment (Assignment, finish))

type Schedule = [(Machine, [Operation])]
type Solution = [Assignment]

calculateSolution :: [Job] -> Schedule -> Solution
calculateSolution js s =
  foldl (\a x -> a ++ x) [] $ map (calculateMachineSolution js) s

calculateMachineSolution :: [Job] -> (Machine, [Operation]) -> Solution
calculateMachineSolution js (m, ops) = foldl mkAssignment [] ops
  where mkAssignment [] op = [Assignment (jobArrival + duration op) op m]
          where jobArrival = arrival $ parentOf js op
        mkAssignment acc op = (Assignment (beginTime + duration op) op m):acc
          where finishOfPrev = finish $ last acc
                jobArrival = arrival $ parentOf js op
                beginTime = max finishOfPrev jobArrival
