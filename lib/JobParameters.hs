module JobParameters
  ( machineDemand
  ) where

import Data.Maybe (fromMaybe)

import Job (Job)
import Operation (Operation, duration)
import QueueAlgorithms (run, restartless, contextFreeQueueAlgorithm)
import Machine (ordinaryMachines)
import Solution (totalFlow)

machineDemand :: (Job, [Operation]) -> Int
machineDemand (j, ops)
  | aproxMDemand >= 0 && aproxMDemand < startingMachinesNum && fastMDemand <= aproxMDemand = fastMDemand
  | otherwise = mDemand
  where fastMDemand = keepTFlowOnFewerMachines maxAllowedTFlow aproxMDemand
        aproxMDemand = idealMDemand + (startingMachinesNum `div` 100)
        idealMDemand = ceiling (fromIntegral (sum opDurations) / fromIntegral maxAllowedTFlow :: Double)
        mDemand = keepTFlowOnFewerMachines maxAllowedTFlow startingMachinesNum
        maxAllowedTFlow = maximum opDurations
        startingMachinesNum = length ops - 1
        opDurations = map duration ops
        keepTFlowOnFewerMachines _ 0 = 1
        keepTFlowOnFewerMachines allowedTFlow machinesNum
          | tflow == allowedTFlow = keepTFlowOnFewerMachines allowedTFlow (machinesNum - 1)
          | otherwise = machinesNum + 1
          where tflow = totalFlow [j]
                        $ run restartless algorithm [j] ops machines
                algorithm = fromMaybe (error "algorithm not found") (contextFreeQueueAlgorithm "sjlo")
                machines = ordinaryMachines machinesNum
