module Solution
  ( Solution
  , costs
  , totalFlow
  , averageFlow
  , mStretch
  , tStretch
  , wStretch
  , pStretch
  , flow
  ) where

import qualified Data.IntMap.Strict as Map

import Job (Job (arrival, uuid))
import Assignment (Assignment (finish, operation))
import Operation (parent, duration)
import Machine (Machine)

type Solution = [Assignment]

totalFlow :: [Job] -> [Assignment] -> Int
totalFlow js as = total $ costs js as flow

averageFlow :: [Job] -> [Assignment] -> Float
averageFlow js as = average $ costs js as flow

costs :: [Job] -> [Assignment] -> (Job -> [Assignment] -> a) -> [(a, Job)] -- TODO: fix funs due to impr
costs js as cost = map (\(j, as') -> (cost j as', j)) jAs
  where jAs = map (\(j, as') -> (head [j' | j' <- js, uuid j' == j], as')) jAs'
        jAs' = Map.toList $ foldl constructJAsMap Map.empty as
        constructJAsMap acc a = Map.insertWith (\_ as' -> a:as') (parent $ operation a) [a] acc

total :: (Num a) => [(a, Job)] -> a
total cjs = sum $ map fst cjs

average :: (Fractional a1, Real a2) => [(a2, Job)] -> a1
average cjs = sum (map (realToFrac . fst) cjs) / fromIntegral (length cjs)

flow :: Job -> [Assignment] -> Int
flow j as = jEnd - jBegin
  where jEnd = maximum [finish a | a <- as, uuid j == (parent . operation) a]
        jBegin = arrival j

mStretch :: Job -> [Assignment] -> Float
mStretch j as = fromIntegral flow' / fromIntegral maxP
  where flow' = flow j as
        maxP = maximum operationDurations
        operationDurations = [(duration . operation) a | a <- as, uuid j == (parent . operation) a]

tStretch :: Job -> [Assignment] -> Float
tStretch j as = fromIntegral flow' / fromIntegral totalP
  where flow' = flow j as
        totalP = sum operationDurations
        operationDurations = [(duration . operation) a | a <- as, uuid j == (parent . operation) a]

wStretch :: [Machine] -> Job -> [Assignment] -> Float
wStretch ms j as = fromIntegral flow' / distributedWeight
  where flow' = flow j as
        distributedWeight = fromIntegral (sum operationDurations) / fromIntegral (length ms)
        operationDurations = [(duration . operation) a | a <- as, uuid j == (parent . operation) a]

pStretch :: [(Int, Job)] -> Job -> [Assignment] -> Float
pStretch fopts j as = fromIntegral flow' / fromIntegral fopt
  where flow' = flow j as
        fopt = fst $ head $ filter ((==j) . snd) fopts
