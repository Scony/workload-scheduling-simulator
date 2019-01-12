module Main where

import System.Environment
import System.IO
import Data.List (sortBy)

import Input
import OfflineAlgorithms
import Machine
import Solution (totalFlow, costs, flow)
import Schedule (calculateSolution)
import QueueAlgorithms (so, fifo, sjlo)
import Job (arrival)
import Validator (validateSolution)

replace a b s = map (\x -> if x == a then b else x) s
mkLines x = map (replace '_' ' ') $ words $ replace ' ' '_' x

main :: IO ()
main = do
  stdin <- getContents
  args <- getArgs

  let algorithm = args !! 0 :: String
  let machinesNum = read $ args !! 1 :: Int
  let machines = ordinaryMachines machinesNum

  let (jobs, operations) = (parseInstanceV2 . mkLines) stdin
  let jobsNum = length jobs
  let operationsNum = length operations

  hPutStrLn stderr $ "> machines: " ++ show machinesNum
  hPutStrLn stderr $ "> jobs: " ++ show jobsNum
  hPutStrLn stderr $ "> operations: " ++ show operationsNum
  hPutStrLn stderr $ "> algorithm: " ++ algorithm

  let scheduleAlgorithm alg = costs jobs solution flow
        where solution = validateSolution jobs operations
                         $ calculateSolution jobs
                         $ alg jobs operations machines
  let queueAlgorithm alg = costs jobs solution flow
        where solution = validateSolution jobs operations $ alg jobs operations machines
  let cjsInOrder cjs = sortBy (\(_, j1) (_, j2) -> compare (arrival j1) (arrival j2)) cjs
  let jobFlows cjs = mapM_ (\(c, _) -> putStrLn $ show c) (cjsInOrder cjs)

  case algorithm of
    "allin1"
      -> jobFlows $ scheduleAlgorithm allInOne
    "opt"
      -> jobFlows $ scheduleAlgorithm (opt totalFlow)
    "worst"
      -> jobFlows $ scheduleAlgorithm (worst totalFlow)
    "so"
      -> jobFlows $ queueAlgorithm so
    "fifo"
      -> jobFlows $ queueAlgorithm fifo
    "sjlo"
      -> jobFlows $ queueAlgorithm sjlo
    _
      -> putStrLn "choose algorithm!!"
