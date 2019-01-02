module Algorithm
  ( allInOne
  , opt
  , worst
  ) where

import Data.List (permutations, elemIndex)

import Job (Job)
import Operation (Operation)
import Machine (Machine)
import Utils (slice)
import Solution (calculateAssignments, calculateJobFlows, calculateJobsTotalFlow)

allInOne :: [Job] -> [Operation] -> [Machine] -> [(Machine, [Operation])]
allInOne js ops ms = [(ms !! 0, ops)]

opt :: [Job] -> [Operation] -> [Machine] -> [(Machine, [Operation])]
opt js ops ms = pickWeightedSolution js ops ms minimum

worst :: [Job] -> [Operation] -> [Machine] -> [(Machine, [Operation])]
worst js ops ms = pickWeightedSolution js ops ms maximum

pickWeightedSolution :: [Job] -> [Operation] -> [Machine] -> ([Int] -> Int) -> [(Machine, [Operation])]
pickWeightedSolution js ops ms fun = snd $ (filter ((==bestWeight) . fst) weightedSolutions') !! 0
  where weightedSolutions' = weightedSolutions js ops ms
        bestWeight = fun $ map fst weightedSolutions'

weightedSolutions :: [Job] -> [Operation] -> [Machine] -> [(Int, [(Machine, [Operation])])]
weightedSolutions js ops ms = map (\x -> (eval x, x)) solutions
  where
    solutions = map (zip ms) $ possibilities ops $ length ms
    eval = calculateJobsTotalFlow
      . calculateJobFlows js
      . calculateAssignments js

possibilities :: [a] -> Int -> [[[a]]]
possibilities xs subsetsNum = process xs
  where process = map (\x -> x ++ replicate (subsetsNum - length x) [])
          . concat
          . map superPermutations
          . filter (\x -> length x <= subsetsNum)
          . partitions

superPermutations :: [[a]] -> [[[a]]]
superPermutations p = cartProdN $ map permutations p
  where cartProdN = sequence

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = expand x $ partitions xs
  where expand :: a -> [[[a]]] -> [[[a]]]
        expand x ys = concatMap (extend x) ys

        extend :: a -> [[a]] -> [[[a]]]
        extend x [] = [[[x]]]
        extend x (y:ys) = ((x:y):ys) : map (y:) (extend x ys)
