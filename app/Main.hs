module Main where

import System.Environment

import Input
import OfflineAlgorithms
import Machine
import Solution
import Schedule (calculateSolution)

replace a b s = map (\x -> if x == a then b else x) s
mkLines x = map (replace '_' ' ') $ words $ replace ' ' '_' x

main :: IO ()
main = do
  stdin <- getContents
  args <- getArgs

  let machinesNum = read $ args !! 0 :: Int
  let machines = ordinaryMachines machinesNum

  let (jobs, operations) = (parseInstanceV2 . mkLines) stdin
  let jobsNum = length jobs
  let operationsNum = length operations

  putStrLn $ "machines: " ++ show machinesNum
  putStrLn $ "jobs: " ++ show jobsNum
  putStrLn $ "operations: " ++ show operationsNum

  putStrLn $ ""

  let evaluateSchedule js = calculateJobsTotalFlow
        . calculateJobFlows js
        . calculateSolution js

  putStrLn $ "allInOne: " ++ show (evaluateSchedule jobs $ allInOne jobs operations machines)
  putStrLn $ "opt: " ++ show (evaluateSchedule jobs $ opt jobs operations machines)
  putStrLn $ "worst: " ++ show (evaluateSchedule jobs $ worst jobs operations machines)
