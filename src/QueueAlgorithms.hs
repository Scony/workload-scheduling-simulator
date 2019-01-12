module QueueAlgorithms
  ( assignInTimeFrame
  , so
  , sjlo
  , sjlo'
  , fifo
  ) where

import Data.List (sortBy)
import Control.Exception (assert)

import Data.List.Extra (nubOrdBy)

import Job (Job, uuid, arrival)
import Operation (Operation, parent, duration)
import Machine (Machine)
import Assignment (Assignment (Assignment))

type Queue = [Operation]
type Time = Int
type MachineState = (Machine, Maybe (Operation, Time))

fifo :: [Job] -> [Operation] -> [Machine] -> [Assignment]
fifo js ops ms = run fifo' js ops ms

so :: [Job] -> [Operation] -> [Machine] -> [Assignment]
so js ops ms = run so' js ops ms

sjlo :: [Job] -> [Operation] -> [Machine] -> [Assignment]
sjlo js ops ms = run sjlo' js ops ms

so' :: [Operation] -> Queue
so' ops = sortBy (\l r -> compare (duration l) (duration r)) ops

fifo' :: [Operation] -> Queue
fifo' ops = ops

sjlo' :: [Operation] -> Queue
sjlo' ops = sortBy cmp ops
  where cmp l r
          | (parent l) == (parent r) = compare (duration r) (duration l) -- lo
          | otherwise = compare (duration' $ parent l) (duration' $ parent r) -- sj
        duration' j = [d | (j', d) <- jobDurations, j' == j] !! 0
        jobDurations = map (\j -> (j, jobDuration j)) jobs
        jobDuration j = sum [duration o | o <- ops, j == parent o]
        jobs = nubOrdBy (\l r -> compare l r) jobs'
        jobs' = map parent ops

run :: ([Operation] -> Queue) -> [Job] -> [Operation] -> [Machine]
    -> [Assignment]
run alg js ops ms = run' alg (-1) sortedJops emptyMachines []
  where
    sortedJops = sortBy (\(j1, _) (j2, _) -> compare (arrival j1) (arrival j2)) jops
    jops = map (\j -> (j, [op | op <- ops, uuid j == parent op])) js
    emptyMachines = map (\x -> (x, Nothing)) ms

run' :: ([Operation] -> Queue) -> Time -> [(Job, [Operation])] -> [MachineState] -> Queue
     -> [Assignment]
run' _ t [] mops q = as
  where
    (_, _, as) = assignInTimeFrame mops q t maxBound
run' alg t jops mops q = as ++ run' alg newT newJops newMops newQ'
  where
    newJops = filter ((/=newT) . arrival . fst) jops
    newQ' = alg opsToProcess
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
