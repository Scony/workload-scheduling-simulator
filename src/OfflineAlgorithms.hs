module OfflineAlgorithms
  ( allInOne
  , opt
  , worst
  ) where

import Data.List (permutations)

import Job (Job)
import Operation (Operation)
import Machine (Machine)
import Schedule (Schedule, calculateSolution)
import Assignment (Assignment)

allInOne :: [Job] -> [Operation] -> [Machine] -> Schedule
allInOne js ops ms = [(head ms, ops)]

opt :: (Ord a) => ([Job] -> [Assignment] -> a) -> [Job] -> [Operation] -> [Machine] -> Schedule
opt fun js ops ms = bestSchedule minimum $ weightedSchedules fun js ops ms

worst :: (Ord a) => ([Job] -> [Assignment] -> a) -> [Job] -> [Operation] -> [Machine] -> Schedule
worst fun js ops ms = bestSchedule maximum $ weightedSchedules fun js ops ms

bestSchedule :: (Eq a) => ([a] -> a) -> [(a, Schedule)] -> Schedule
bestSchedule fun ss = snd $ head $ filter ((==bestWeight) . fst) ss
  where bestWeight = fun $ map fst ss

weightedSchedules :: ([Job] -> [Assignment] -> a) -> [Job] -> [Operation] -> [Machine]
                  -> [(a, Schedule)]
weightedSchedules fun js ops ms = map (\x -> (eval x, x)) schedules
  where schedules = map (zip ms) $ possibilities ops $ length ms
        eval = fun js
          . calculateSolution js

possibilities :: [a] -> Int -> [[[a]]]
possibilities xs subsetsNum = process xs
  where process = map (\x -> x ++ replicate (subsetsNum - length x) [])
          . concatMap superPermutations
          . filter (\x -> length x <= subsetsNum)
          . partitions

superPermutations :: [[a]] -> [[[a]]]
superPermutations p = cartProdN $ map permutations p
  where cartProdN = sequence

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = expand x $ partitions xs
  where expand :: a -> [[[a]]] -> [[[a]]]
        expand x = concatMap (extend x)

        extend :: a -> [[a]] -> [[[a]]]
        extend x [] = [[[x]]]
        extend x (y:ys) = ((x:y):ys) : map (y:) (extend x ys)
