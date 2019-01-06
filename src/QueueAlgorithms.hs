module QueueAlgorithms where

import Data.List (sortBy)
import Control.Exception(assert)

import Job (Job, uuid, arrival)
import Operation (Operation, parent)
import Machine (Machine)
import Assignment (Assignment)

type Queue = [Operation]
type Time = Int

rand :: [Job] -> [Operation] -> [Machine] -> [Assignment]
rand js ops ms = run rand' js ops ms

run :: ([Operation] -> Queue) -> [Job] -> [Operation] -> [Machine]
    -> [Assignment]
run alg js ops ms = run' (-1) sortedJops emptyMachines []
  where
    sortedJops = sortBy (\(j1, _) (j2, _) -> compare (arrival j1) (arrival j2)) jops
    jops = map (\j -> (j, [op | op <- ops, uuid j == parent op])) js
    emptyMachines = map (\x -> (x, Nothing)) ms

run' :: Time -> [(Job, [Operation])] -> [(Machine, Maybe (Operation, Time))] -> Queue
     -> [Assignment]
run' t [] mops q = as
  where
    (_, _, as) = assignInTimeFrame mops q t maxBound
run' t jops mops q = as ++ run' newT newJops newMops newQ'
  where
    newJops = filter ((/=newT) . arrival . fst) jops
    newQ' = rand' opsToProcess  -- TODO: pass fun
    opsToProcess = newQ ++ newOps
    newOps = concat [ops | (j, ops) <- jops, arrival j == newT]
    (newMops, newQ, as) = assignInTimeFrame mops q t newT
    newT = (arrival . fst . (!! 0)) jops

assignInTimeFrame :: [(Machine, Maybe (Operation, Time))] -> Queue -> Time -> Time
                  -> ([(Machine, Maybe (Operation, Time))], Queue, [Assignment])
assignInTimeFrame mops q from until = assert workIsOngoing ([], [], [])
  where
    workIsOngoing = and $ map workIsOngoing' mops
    workIsOngoing' (_, op) = case op of
      Just (_, t) -> t > from
      Nothing -> True

rand' :: [Operation] -> Queue
rand' ops = reverse ops