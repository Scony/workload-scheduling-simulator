module Main where

import System.Environment

import Input
import OfflineAlgorithms
import Machine
import Solution (totalFlow, averageFlow)
import Schedule (calculateSolution)
import QueueAlgorithms (so)

replace a b s = map (\x -> if x == a then b else x) s
mkLines x = map (replace '_' ' ') $ words $ replace ' ' '_' x

main :: IO ()
main = do
  stdin <- getContents
  args <- getArgs

  let algorithm = args !! 0 :: String
  putStrLn algorithm
  let machinesNum = read $ args !! 1 :: Int
  let machines = ordinaryMachines machinesNum

  let (jobs, operations) = (parseInstanceV2 . mkLines) stdin
  let jobsNum = length jobs
  let operationsNum = length operations

  putStrLn $ "machines: " ++ show machinesNum
  putStrLn $ "jobs: " ++ show jobsNum
  putStrLn $ "operations: " ++ show operationsNum

  putStrLn $ ""

  case algorithm of
    "allin1"
      -> putStrLn $ "allIn1: " ++ show (evaluateSchedule jobs $ allInOne jobs operations machines)
    "opt"
      -> putStrLn $ "opt: " ++ show (evaluateSchedule jobs $ opt totalFlow jobs operations machines)
    "worst"
      -> putStrLn $ "worst: " ++ show (evaluateSchedule jobs $ worst totalFlow jobs operations machines)
    "so"
      -> putStrLn $ "so: " ++ show (averageFlow jobs $ so jobs operations machines)
    _
      -> putStrLn "choose algorithm!!"

evaluateSchedule js = averageFlow js . calculateSolution js
