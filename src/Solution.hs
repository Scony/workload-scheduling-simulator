module Solution
  ( Solution
  , costs
  , totalFlow
  , averageFlow
  , mStretch
  , tStretch
  , wStretch
  , pStretch
  ) where

import Job (Job (arrival, uuid))
import Assignment (Assignment (finish, operation))
import Operation (parent, duration)
import Machine (Machine)

type Solution = [Assignment]

totalFlow :: [Job] -> [Assignment] -> Int
totalFlow js as = total $ costs js as flow

averageFlow :: [Job] -> [Assignment] -> Float
averageFlow js as = average $ costs js as flow

costs :: [Job] -> [Assignment] -> (Job -> [Assignment] -> a) -> [(a, Job)]
costs js as cost = map (\x -> (cost x as, x)) js

total :: (Num a) => [(a, Job)] -> a
total cjs = sum $ map fst cjs

-- average :: (Fractional a) => [(a, Job)] -> a
average cjs = (sum $ map (realToFrac . fst) cjs) / fromIntegral (length cjs)

flow :: Job -> [Assignment] -> Int
flow j as = jEnd - jBegin
  where
    jEnd = maximum [finish a | a <- as, uuid j == (parent . operation) a]
    jBegin = arrival j

mStretch :: Job -> [Assignment] -> Float
mStretch j as = fromIntegral flow' / fromIntegral maxP
  where
    flow' = flow j as
    maxP = maximum operationDurations
    operationDurations = [(duration . operation) a | a <- as, uuid j == (parent . operation) a]

tStretch :: Job -> [Assignment] -> Float
tStretch j as = fromIntegral flow' / fromIntegral totalP
  where
    flow' = flow j as
    totalP = sum operationDurations
    operationDurations = [(duration . operation) a | a <- as, uuid j == (parent . operation) a]

wStretch :: [Machine] -> Job -> [Assignment] -> Float
wStretch ms j as = fromIntegral flow' / distributedWeight
  where
    flow' = flow j as
    distributedWeight = (fromIntegral $ sum operationDurations) / (fromIntegral $ length ms)
    operationDurations = [(duration . operation) a | a <- as, uuid j == (parent . operation) a]

pStretch :: [(Int, Job)] -> Job -> [Assignment] -> Float
pStretch fopts j as = fromIntegral flow' / fromIntegral fopt
  where
    flow' = flow j as
    fopt = fst $ (filter ((==j) . snd) fopts) !! 0
