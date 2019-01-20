module QueueAlgorithms
  ( assignInTimeFrame, run, lookupByName, restartless, restartful
  , so, lo, fifo, lifo, sjlo, sjso, ljso, ljlo, rrso, rrlo, sjmd, sjmdr, md, mdr
  , QueueAlgorithm
  ) where

import Data.List (sortBy)
import qualified Data.IntMap.Strict as Map
import Utils (assert)

import Job (Job, uuid, arrival)
import Operation (Operation (Operation), parent, duration, uuid)
import Machine (Machine)
import Assignment (Assignment (Assignment))

type Queue = [Operation]
type Time = Int
type MachineState = (Machine, Maybe (Operation, Time))
type QueueAlgorithm = [Operation] -> Queue
type Runner =
  QueueAlgorithm -> Time -> [(Job, [Operation])] -> [MachineState] -> Queue -> [Assignment]

lookupByName :: String -> Maybe QueueAlgorithm
lookupByName name = case name of
  "so" -> Just so
  "lo" -> Just lo
  "fifo" -> Just fifo
  "lifo" -> Just lifo
  "sjlo" -> Just sjlo
  "sjso" -> Just sjso
  "ljso" -> Just ljso
  "ljlo" -> Just ljlo
  "rrso" -> Just rrso
  "rrlo" -> Just rrlo
  "sjmd" -> Just sjmd
  "sjmdr" -> Just sjmdr
  "md" -> Just md
  "mdr" -> Just mdr
  _ -> Nothing

so :: QueueAlgorithm
so = sortBy (\l r -> compare (duration l) (duration r))

lo :: QueueAlgorithm
lo = reverse . so

fifo :: QueueAlgorithm
fifo ops = ops

lifo :: QueueAlgorithm
lifo = reverse . fifo

sjx :: QueueAlgorithm -> [Operation] -> Queue
sjx opAlg ops = concatMap snd $ sortBy jCmp dOps
  where dOps = map (\(_, ops') -> (sum $ map duration ops', opAlg ops')) jOps
        jCmp l r = compare (fst l) (fst r)           -- sj
        jOps = Map.toList
               $ foldl (\acc o -> Map.insertWith (\_ os -> o:os) (parent o) [o] acc) Map.empty ops

sjlo :: QueueAlgorithm
sjlo = sjx lo

sjso :: QueueAlgorithm
sjso = sjx so

ljso :: QueueAlgorithm
ljso = reverse . sjlo

ljlo :: QueueAlgorithm
ljlo = reverse . sjso

md :: QueueAlgorithm
md ops = (map snd
          . sortBy (\l r -> compare (fst l) (fst r))
          . map (\(ix, o) -> (abs(ix - midIx), o))
          . zip [0..]
          . sortBy (\l r -> compare (duration l) (duration r)))
          ops
  where midIx = floor $ fromIntegral (length ops) / 2 :: Int

mdr :: QueueAlgorithm
mdr = reverse . md

sjmd :: QueueAlgorithm
sjmd = sjx md

sjmdr :: QueueAlgorithm
sjmdr = sjx mdr

rrx :: QueueAlgorithm -> [Operation] -> Queue
rrx opAlg ops = map snd $ sortBy (\a b -> compare (fst a) (fst b)) $ concatMap (zip [1..]) orderedOpss
  where orderedOpss = map opAlg opss
        opss = map snd $ Map.toList
               $ foldl (\acc o -> Map.insertWith (\_ os -> o:os) (parent o) [o] acc) Map.empty ops

rrso :: QueueAlgorithm
rrso = rrx so

rrlo :: QueueAlgorithm
rrlo = rrx lo

run :: Runner -> QueueAlgorithm -> [Job] -> [Operation] -> [Machine]
    -> [Assignment]
run runner alg js ops ms = runner alg (-1) sortedJops emptyMachines []
  where
    sortedJops = sortBy (\(j1, _) (j2, _) -> compare (arrival j1) (arrival j2)) jOps
    jOps = map (\(j, ops') -> (head [j' | j' <- js, Job.uuid j' == j], ops')) jOps'
    jOps' = Map.toList $ foldl constructJOpsMap Map.empty ops
    constructJOpsMap acc o = Map.insertWith (\_ ops' -> o:ops') (parent o) [o] acc
    emptyMachines = map (\x -> (x, Nothing)) ms

restartless :: QueueAlgorithm -> Time -> [(Job, [Operation])] -> [MachineState] -> Queue
            -> [Assignment]
restartless _ t [] mops q = as
  where
    (_, _, as) = assignInTimeFrame mops q t maxBound
restartless alg t jops mops q = as ++ restartless alg newT newJops newMops newQ'
  where
    newJops = filter ((/=newT) . arrival . fst) jops
    newQ' = alg opsToProcess
    opsToProcess = newQ ++ newOps
    newOps = concat [ops | (j, ops) <- jops, arrival j == newT]
    (newMops, newQ, as) = assignInTimeFrame mops q t newT
    newT = (arrival . fst . head) jops

restartful :: QueueAlgorithm -> Time -> [(Job, [Operation])] -> [MachineState] -> Queue
            -> [Assignment]
restartful _ t [] mops q = as
  where
    (_, _, as) = assignInTimeFrame mops q t maxBound
restartful alg t jops mops q = as ++ restartful alg newT newJops newMops' newQ'
  where
    newJops = filter ((/=newT) . arrival . fst) jops
    (newMops', newQ') = runAlgorithmWithRestarts alg newT newMops opsToProcess
    opsToProcess = newQ ++ newOps
    newOps = concat [ops | (j, ops) <- jops, arrival j == newT]
    (newMops, newQ, as) = assignInTimeFrame mops q t newT
    newT = (arrival . fst . head) jops

runAlgorithmWithRestarts :: QueueAlgorithm -> Time -> [MachineState] -> [Operation]
                         -> ([MachineState], Queue)
runAlgorithmWithRestarts alg t mops ops = (mopsAfterResets, q)
  where
    mopsAfterResets = map resetMachineIfNeeded mops
    resetMachineIfNeeded (m, opt) = case opt of
      Just (o, _) -> if o `elem` opsToReset then (m, Nothing) else (m, opt)
      Nothing -> (m, opt)
    q = map (\o -> if Operation.uuid o < 0 then unFakeOp o else o) q'
    opsToReset = [unFakeOp o | o <- q', Operation.uuid o < 0]
    q' = filter (`notElem` resetFreeOpsAfterStage2) opsStage2
    resetFreeOpsAfterStage2 = [o | o <- take mNumForStage2 opsStage2, Operation.uuid o < 0]
    opsStage2 = alg $ fakeOpsForStage2 ++ ops
    fakeOpsForStage2 = filter (`notElem` resetFreeOpsAfterStage1) fakeOpsForStage2'
    fakeOpsForStage2' = [fakeOp o (duration o) | (_, Just (o, _)) <- mops]
    mNumForStage2 = mNum - length resetFreeOpsAfterStage1
    resetFreeOpsAfterStage1 = [o | o <- take mNum opsStage1, Operation.uuid o < 0]
    opsStage1 = alg $ fakeOps ++ ops
    fakeOps = [fakeOp o (finishTime-t) | (_, Just (o, finishTime)) <- mops]
    fakeOp (Operation p u k o _ c) fakeD = Operation p (-u) k o fakeD c
    unFakeOp (Operation p u k o d c) = Operation p (-u) k o d c
    mNum = length mops

assignInTimeFrame :: [(Machine, Maybe (Operation, Time))] -> Queue -> Time -> Time
                  -> ([(Machine, Maybe (Operation, Time))], Queue, [Assignment])
assignInTimeFrame mops q from until = assert workIsOngoing "WiO" assignInTimeFrame' mops q from until
  where
    workIsOngoing = all workIsOngoing' mops
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
  | releaseableMachinesExist = (mopsWithFreedOnes', op:ops, newAs'')
  | otherwise = (mops, op:ops, [])
  where
    freeMachineExists = any isMachineFree mops
    isMachineFree (m, op') = case op' of
      Just _ -> False
      Nothing -> True
    mopsWithFilledOne mops' Nothing = mops'
    mopsWithFilledOne ((m, op'):mops') (Just op'') = case op' of
      Just _ -> (m, op') : mopsWithFilledOne mops' (Just op'')
      Nothing -> (m, Just (op'', from + duration op'')):mopsWithFilledOne mops' Nothing
    freeMachineCanExist = any isMachineFreeable mops
    isMachineFreeable (_, op') = case op' of
      Just (_, t) -> t < until
      Nothing -> False
    (newMops, newQ, newAs) = assignInTimeFrame' mopsWithFreedOnes (op:ops) newFrom until
    (mopsWithFreedOnes, _, newAs') = assignInTimeFrame' mops [] (-1) newFrom
    newFrom = minimum $ foldl fetchT [] mops
    fetchT acc (_, op') = case op' of
      Just (_, t) -> t:acc
      Nothing -> acc
    releaseableMachinesExist = any isMachineReleasable mops
    isMachineReleasable (_, op') = case op' of
      Just (_, t) -> t == until
      Nothing -> False
    (mopsWithFreedOnes', _, newAs'') = assignInTimeFrame' mops [] (-1) until
