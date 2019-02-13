module JobParameters
  ( machineDemand
  ) where

import Data.Maybe (fromMaybe)

import Job (Job)
import Operation (Operation, duration)
import qualified QueueAlgorithms as QAlgorithms
import Machine (ordinaryMachines)
import Solution (totalFlow)

machineDemand :: (Job, [Operation]) -> Int
machineDemand (j, ops) = keepTFlowOnFewerMachines maxAllowedTFlow startingMachinesNum
  where maxAllowedTFlow = maximum $ map duration ops
        startingMachinesNum = length ops
        keepTFlowOnFewerMachines _ 0 = 1
        keepTFlowOnFewerMachines allowedTFlow machinesNum
          | tflow == allowedTFlow = keepTFlowOnFewerMachines allowedTFlow (machinesNum - 1)
          | otherwise = machinesNum + 1
          where tflow = totalFlow [j]
                        $ QAlgorithms.run QAlgorithms.restartless algorithm [j] ops machines
                algorithm = fromMaybe (error "algorithm not found") (QAlgorithms.lookupByName "sjlo")
                machines = ordinaryMachines machinesNum
