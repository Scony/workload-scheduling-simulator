module QueueAlgorithms
  ( assignInTimeFrame
  ) where

import Data.List (sortBy)
import Control.Exception (assert)

import Job (Job, uuid, arrival)
import Operation (Operation, parent, duration)
import Machine (Machine)
import Assignment (Assignment (Assignment))

type Queue = [Operation]
type Time = Int

rand :: [Job] -> [Operation] -> [Machine] -> [Assignment]
rand js ops ms = run rand' js ops ms

rand' :: [Operation] -> Queue
rand' ops = reverse ops

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
assignInTimeFrame mops q from until = assert workIsOngoing assignInTimeFrame' mops q from until
  where
    workIsOngoing = and $ map workIsOngoing' mops
    workIsOngoing' (_, op) = case op of
      Just (_, t) -> t > from
      Nothing -> True

assignInTimeFrame' :: [(Machine, Maybe (Operation, Time))] -> Queue -> Time -> Time
                   -> ([(Machine, Maybe (Operation, Time))], Queue, [Assignment])
assignInTimeFrame' mops [] _ until = (newMops, [], as)
  where
    newMops = map releaseMachine mops
    releaseMachine (m, op) = case op of
      Just (op', t) -> if t <= until then (m, Nothing) else (m, Just (op', t))
      Nothing -> (m, Nothing)
    as = foldl assign [] mops
    assign acc (m, op) = case op of
      Just (op', t) -> if t <= until then acc ++ [Assignment t op' m] else acc
      Nothing -> acc
assignInTimeFrame' mops (op:ops) from until
  | freeMachineExists = assignInTimeFrame' (mopsWithFilledOne mops (Just op)) ops from until
  | freeMachineCanExist = (newMops, newQ, newAs' ++ newAs)
  | releaseableMachinesExist = (mopsWithFreedOnes', (op:ops), newAs'')
  | otherwise = (mops, (op:ops), [])
  where
    freeMachineExists = or $ map isMachineFree mops
    isMachineFree (m, op') = case op' of
      Just _ -> False
      Nothing -> True
    mopsWithFilledOne mops' Nothing = mops'
    mopsWithFilledOne ((m, op'):mops') (Just op'') = case op' of
      Just _ -> (m, op'):(mopsWithFilledOne mops' (Just op''))
      Nothing -> (m, Just (op'', from + duration op'')):(mopsWithFilledOne mops' Nothing)
    freeMachineCanExist = or $ map isMachineFreeable mops
    isMachineFreeable (_, op') = case op' of
      Just (_, t) -> t < until
      Nothing -> False
    (newMops, newQ, newAs) = assignInTimeFrame' mopsWithFreedOnes (op:ops) newFrom until
    (mopsWithFreedOnes, _, newAs') = assignInTimeFrame' mops [] (-1) newFrom
    newFrom = minimum $ foldl fetchT [] mops
    fetchT acc (_, op') = case op' of
      Just (_, t) -> t:acc
      Nothing -> acc
    releaseableMachinesExist = or $ map isMachineReleasable mops
    isMachineReleasable (_, op') = case op' of
      Just (_, t) -> t == until
      Nothing -> False
    (mopsWithFreedOnes', _, newAs'') = assignInTimeFrame' mops [] (-1) until
