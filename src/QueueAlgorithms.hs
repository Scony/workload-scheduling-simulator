module QueueAlgorithms
  ( assignInTimeFrame, run, lookupByName, restartless, restartful
  , sjmd
  , QueueAlgorithm
  ) where

import Data.List (sortBy)
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map

import Utils (assert, mapJs2Ops, mapJs2Ops')
import Job (Job, arrival, uuid)
import Operation (Operation (Operation), parent, duration, uuid)
import Machine (Machine)
import Assignment (Assignment (Assignment))
import Solution (totalFlow)

type Queue = [Operation]
type Time = Int
type MachineState = (Machine, Maybe (Operation, Time))
type JOpsMap = Map.Map Job [Operation]
type JOpssLeft = [(Job, [Operation])]
type QueueAlgorithm = Time -> [MachineState] -> JOpsMap -> [Operation] -> Queue
type RestartPolicy
  = QueueAlgorithm -> JOpsMap -> Time -> [MachineState] -> [Operation] -> ([MachineState], Queue)

lookupByName :: String -> Maybe QueueAlgorithm
lookupByName name = case name of
  "so" -> Just (adjust so)
  "lo" -> Just (adjust lo)
  "fifo" -> Just (adjust fifo)
  "lifo" -> Just (adjust lifo)
  "sjlo" -> Just (adjust sjlo)
  "sjso" -> Just (adjust sjso)
  "ljso" -> Just (adjust' ljso)
  "ljlo" -> Just (adjust' ljlo)
  "rrso" -> Just (adjust rrso)
  "rrlo" -> Just (adjust rrlo)
  "sjmd" -> Just (adjust sjmd)
  "sjmdr" -> Just (adjust sjmdr)
  "md" -> Just (adjust md)
  "mdr" -> Just (adjust mdr)
  "smjlo" -> Just (adjust smjlo)
  "smjso" -> Just (adjust smjso)
  "lmjlo" -> Just (adjust lmjlo)
  "lmjso" -> Just (adjust lmjso)
  "sjlo1m" -> Just sjlo1m
  "sjso1m" -> Just sjso1m
  _ -> Nothing
  where adjust alg _ _ _ = alg
        adjust' alg _ _ = alg

so :: [Operation] -> Queue
so = sortBy (\l r -> compare (duration l) (duration r))

lo :: [Operation] -> Queue
lo = reverse . so

fifo :: [Operation] -> Queue
fifo ops = ops

lifo :: [Operation] -> Queue
lifo = reverse . fifo

xjx :: ([Int] -> Int) -> ([Operation] -> Queue) -> [Operation] -> Queue
xjx criterion opAlg ops = concatMap snd $ sortBy sj dOps
  where dOps = map (\(_, ops') -> (criterion $ map duration ops', opAlg ops')) todoJOpss
        sj l r = compare (fst l) (fst r)
        todoJOpss = IMap.toList $ mapJs2Ops' ops

sjx :: ([Operation] -> Queue) -> [Operation] -> Queue
sjx = xjx sum

sjlo :: [Operation] -> Queue
sjlo = sjx lo

sjso :: [Operation] -> Queue
sjso = sjx so

ljx :: ([Operation] -> Queue) -> JOpsMap -> [Operation] -> Queue
ljx opAlg jOpsMap ops = concatMap snd $ sortBy lj dOpss
  where dOpss = map (\(j, ops') -> (jDuration j, opAlg ops')) todoJOpss
        lj l r = compare (fst r) (fst l)
        jDuration j = IMap.lookup j jDurationsIMap
        jDurationsIMap = IMap.fromList
                         $ map (\(j, ops') -> (Job.uuid j, sum $ map duration ops'))
                         $ Map.toList
                         jOpsMap
        todoJOpss = IMap.toList $ mapJs2Ops' ops

ljlo :: JOpsMap -> [Operation] -> Queue
ljlo = ljx lo

ljso :: JOpsMap -> [Operation] -> Queue
ljso = ljx so

smjx :: ([Operation] -> Queue) -> [Operation] -> Queue
smjx = xjx maximum

smjlo :: [Operation] -> Queue
smjlo = smjx lo

smjso :: [Operation] -> Queue
smjso = smjx so

lmjlo :: [Operation] -> Queue
lmjlo = reverse . smjso

lmjso :: [Operation] -> Queue
lmjso = reverse . smjlo

-- TODO: pass cost function
sjx1m :: ([Operation] -> Queue) -> Time -> [MachineState] -> JOpsMap -> [Operation]
      -> Queue
sjx1m opAlg t mops jOpsMap ops = if queue1mCost < queue0mCost then queue1m else queue0m
  where queue1mCost = totalFlow jobs queue1mAs
        (_, _, queue1mAs) = assignInTimeFrame mops queue1m t maxBound
        queue0mCost = totalFlow jobs queue0mAs
        (_, _, queue0mAs) = assignInTimeFrame mops queue0m t maxBound
        jobs = map fst $ Map.toList jOpsMap
        queue1m = concatMap opAlg $ concat (take 2 queue) : drop 2 queue
        queue0m = concatMap opAlg queue
        queue = map snd $ sortBy sj dOps
        dOps = map (\(_, ops') -> (sum $ map duration ops', ops')) todoJOpss
        sj l r = compare (fst l) (fst r)
        todoJOpss = IMap.toList $ mapJs2Ops' ops

sjlo1m :: Time -> [MachineState] -> JOpsMap -> [Operation] -> Queue
sjlo1m = sjx1m lo

sjso1m :: Time -> [MachineState] -> JOpsMap -> [Operation] -> Queue
sjso1m = sjx1m so

md :: [Operation] -> Queue
md ops = (map snd
          . sortBy (\l r -> compare (fst l) (fst r))
          . map (\(ix, o) -> (abs(ix - midIx), o))
          . zip [0..]
          . sortBy (\l r -> compare (duration l) (duration r)))
          ops
  where midIx = floor $ fromIntegral (length ops) / 2 :: Int

mdr :: [Operation] -> Queue
mdr = reverse . md

sjmd :: [Operation] -> Queue
sjmd = sjx md

sjmdr :: [Operation] -> Queue
sjmdr = sjx mdr

rrx :: ([Operation] -> Queue) -> [Operation] -> Queue
rrx opAlg ops = map snd $ sortBy (\a b -> compare (fst a) (fst b)) $ concatMap (zip [1..]) orderedOpss
  where orderedOpss = map opAlg opss
        opss = map snd $ IMap.toList
               $ foldl (\acc o -> IMap.insertWith (\_ os -> o:os) (parent o) [o] acc) IMap.empty ops

rrso :: [Operation] -> Queue
rrso = rrx so

rrlo :: [Operation] -> Queue
rrlo = rrx lo

run :: RestartPolicy -> QueueAlgorithm -> [Job] -> [Operation] -> [Machine]
    -> [Assignment]
run restartPolicy alg js ops ms = run' restartPolicy alg jOpsMap (-1) sortedJOpss emptyMachines []
  where
    sortedJOpss = sortBy (\(j1, _) (j2, _) -> compare (arrival j1) (arrival j2)) $ Map.toList jOpsMap
    jOpsMap = mapJs2Ops js ops
    emptyMachines = map (\x -> (x, Nothing)) ms

run' :: RestartPolicy -> QueueAlgorithm -> JOpsMap -> Time -> JOpssLeft -> [MachineState] -> Queue
     -> [Assignment]
run' _ _ _ t [] mops q = as
  where
    (_, _, as) = assignInTimeFrame mops q t maxBound
run' restartPolicy alg jOpsMap t jOpssLeft mops q =
  as ++ run' restartPolicy alg jOpsMap newT newJOpssLeft newMops' newQ'
  where
    newJOpssLeft = filter ((/=newT) . arrival . fst) jOpssLeft
    (newMops', newQ') = restartPolicy alg jOpsMap newT newMops opsToProcess
    opsToProcess = newQ ++ newOps
    newOps = concat [ops | (j, ops) <- jOpssLeft, arrival j == newT]
    (newMops, newQ, as) = assignInTimeFrame mops q t newT
    newT = (arrival . fst . head) jOpssLeft

restartless :: QueueAlgorithm -> JOpsMap -> Time -> [MachineState] -> [Operation]
            -> ([MachineState], Queue)
restartless algorithm jOpsMap t mops ops = (mops, algorithm t mops jOpsMap ops)

restartful :: QueueAlgorithm -> JOpsMap -> Time -> [MachineState] -> [Operation]
           -> ([MachineState], Queue)
restartful alg jOpsMap t mops ops = if qWORestarts == qWRestarts
                                    then outcomeWORestarts
                                    else betterOutcome
  where
    betterOutcome = if costWORestarts < costWRestarts
                    then outcomeWORestarts
                    else outcomeWRestarts
    costWORestarts = totalFlow jobs asWORestarts
    costWRestarts = totalFlow jobs asWRestarts
    (_, _, asWORestarts) = assignInTimeFrame mops qWORestarts t maxBound
    (_, _, asWRestarts) = assignInTimeFrame mopsAfterResets qWRestarts t maxBound
    jobs = map fst $ Map.toList jOpsMap
    outcomeWORestarts = (mops, qWORestarts)
    outcomeWRestarts = (mopsAfterResets, qWRestarts)
    qWORestarts = alg t mops jOpsMap ops
    (mopsAfterResets, qWRestarts) = dumbRestartful alg jOpsMap t mops ops

dumbRestartful :: QueueAlgorithm -> JOpsMap -> Time -> [MachineState] -> [Operation]
               -> ([MachineState], Queue)
dumbRestartful alg jOpsMap t mops ops = (mopsAfterResets, q)
  where
    mopsAfterResets = map resetMachineIfNeeded mops
    resetMachineIfNeeded (m, opt) = case opt of
      Just (o, _) -> if o `elem` opsToReset then (m, Nothing) else (m, opt)
      Nothing -> (m, opt)
    q = map (\o -> if Operation.uuid o < 0 then unFakeOp o else o) q'
    opsToReset = [unFakeOp o | o <- q', Operation.uuid o < 0]
    q' = filter (`notElem` resetFreeOpsAfterStage2) opsStage2
    resetFreeOpsAfterStage2 = [o | o <- take mNumForStage2 opsStage2, Operation.uuid o < 0]
    opsStage2 = alg t mopsForStage2 jOpsMap $ fakeOpsForStage2 ++ ops
    fakeOpsForStage2 = filter (`notElem` resetFreeOpsAfterStage1)
                       [fakeOp o (duration o) | (_, Just (o, _)) <- mops]
    mNumForStage2 = mNum - length resetFreeOpsAfterStage1
    mopsForStage2 = map resetMachineIfNeeded' mops
    resetMachineIfNeeded' (m, opt) = case opt of
      Just (o, _) -> if o `elem` resetFreeOpsAfterStage1' then (m, opt) else (m, Nothing)
      Nothing -> (m, opt)
    resetFreeOpsAfterStage1' = map unFakeOp resetFreeOpsAfterStage1
    resetFreeOpsAfterStage1 = [o | o <- take mNum opsStage1, Operation.uuid o < 0]
    opsStage1 = alg t mopsForStage1 jOpsMap $ fakeOps ++ ops
    mopsForStage1 = map (\(m, _) -> (m, Nothing)) mops
    fakeOps = [fakeOp o (finishTime-t) | (_, Just (o, finishTime)) <- mops]
    fakeOp (Operation p u k o _ c) fakeD = Operation p (-u-1) k o fakeD c
    unFakeOp (Operation p u k o d c) = Operation p (-u-1) k o d c
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
