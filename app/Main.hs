{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hPutStrLn, stderr)
import Data.List (sortBy)

import Options.Generic (ParseRecord, Generic, getRecord)

import Input (parseInstanceV2)
import qualified OfflineAlgorithms
import Machine (ordinaryMachines)
import qualified Solution
import Schedule (calculateSolution)
import qualified QueueAlgorithms as QAlgorithms
import Job (arrival, uuid)
import Validator (validateSolution)
import Operation (parent)

replace a b = map (\x -> if x == a then b else x)
mkLines x = map (replace '_' ' ') $ words $ replace ' ' '_' x

data Arguments
  = Run String Int String     -- running algorithms
  | Algdet String Int         -- approximated algorithm deteriorations
  deriving (Generic, Show)

instance ParseRecord Arguments

main :: IO ()
main = do
  parsedArgs <- getRecord "Multipurpose workload scheduling simulator"
  main' parsedArgs

main' :: Arguments -> IO ()

main' (Run algorithm machinesNum costFunction) = do
  stdin <- getContents

  let machines = ordinaryMachines machinesNum
  let (jobs, operations) = (parseInstanceV2 . mkLines) stdin
  let jobsNum = length jobs
  let operationsNum = length operations

  hPutStrLn stderr $ "> machines: " ++ show machinesNum
  hPutStrLn stderr $ "> jobs: " ++ show jobsNum
  hPutStrLn stderr $ "> operations: " ++ show operationsNum
  hPutStrLn stderr $ "> algorithm: " ++ algorithm
  hPutStrLn stderr $ "> cost function: " ++ costFunction

  let costFun a b = case costFunction of
        "flow" -> fromIntegral $ Solution.flow a b
        "mstretch" -> Solution.mStretch a b
        "tstretch" -> Solution.tStretch a b
        "wstretch" -> Solution.wStretch machines a b
        "apstretch" -> Solution.pStretch approxJobPerfectFlows a b
        _ -> error "choose cost function!!"
        where approxJobPerfectFlows = map approxJobPerfectFlow jobs
              approxJobPerfectFlow j = head $ Solution.costs [j] solution Solution.flow
                where solution = QAlgorithms.run QAlgorithms.sjlo [j] operations' machines
                      operations' = [o | o <- operations, uuid j == parent o]

  let scheduleAlgorithm alg = Solution.costs jobs solution costFun
        where solution = validateSolution jobs operations
                         $ calculateSolution jobs
                         $ alg jobs operations machines
  let queueAlgorithm alg = Solution.costs jobs solution costFun
        where solution = validateSolution jobs operations $ QAlgorithms.run alg jobs operations machines
  let cjsInOrder = sortBy (\(_, j1) (_, j2) -> compare (arrival j1) (arrival j2))
  let jobCosts cjs = mapM_ (\(c, _) -> print c) (cjsInOrder cjs)

  case algorithm of
    "allin1" -> jobCosts $ scheduleAlgorithm OfflineAlgorithms.allInOne
    "opt" -> jobCosts $ scheduleAlgorithm (OfflineAlgorithms.opt Solution.totalFlow)
    "worst" -> jobCosts $ scheduleAlgorithm (OfflineAlgorithms.worst Solution.totalFlow)
    "so" -> jobCosts $ queueAlgorithm QAlgorithms.so
    "lo" -> jobCosts $ queueAlgorithm QAlgorithms.lo
    "fifo" -> jobCosts $ queueAlgorithm QAlgorithms.fifo
    "lifo" -> jobCosts $ queueAlgorithm QAlgorithms.lifo
    "sjlo" -> jobCosts $ queueAlgorithm QAlgorithms.sjlo
    "sjso" -> jobCosts $ queueAlgorithm QAlgorithms.sjso
    "ljso" -> jobCosts $ queueAlgorithm QAlgorithms.ljso
    "ljlo" -> jobCosts $ queueAlgorithm QAlgorithms.ljlo
    "rrso" -> jobCosts $ queueAlgorithm QAlgorithms.rrso
    "rrlo" -> jobCosts $ queueAlgorithm QAlgorithms.rrlo
    "sjmd" -> jobCosts $ queueAlgorithm QAlgorithms.sjmd
    "sjmdr" -> jobCosts $ queueAlgorithm QAlgorithms.sjmdr
    "md" -> jobCosts $ queueAlgorithm QAlgorithms.md
    "mdr" -> jobCosts $ queueAlgorithm QAlgorithms.mdr
    _ -> error "choose algorithm!!"

main' (Algdet algorithm machinesNum) = do
  stdin <- getContents

  let machines = ordinaryMachines machinesNum
  let (jobs, operations) = (parseInstanceV2 . mkLines) stdin
  let jobsNum = length jobs
  let operationsNum = length operations

  hPutStrLn stderr $ "> machines: " ++ show machinesNum
  hPutStrLn stderr $ "> jobs: " ++ show jobsNum
  hPutStrLn stderr $ "> operations: " ++ show operationsNum
  hPutStrLn stderr $ "> algorithm: " ++ algorithm

  let approxJobAlgDets = map approxJobAlgDet jobs
        where approxJobAlgDet j = fromIntegral (fst $ jobUnbiasedFlow j)
                                  / fromIntegral (fst $ approxJobPerfectFlow j)
              approxJobPerfectFlow j = head $ Solution.costs [j] solution Solution.flow
                where solution = QAlgorithms.run QAlgorithms.sjlo [j] operations' machines
                      operations' = [o | o <- operations, uuid j == parent o]
              jobUnbiasedFlow j = head $ Solution.costs [j] solution Solution.flow
                where solution = QAlgorithms.run (qAlgorithmByName algorithm) [j] operations' machines
                      operations' = [o | o <- operations, uuid j == parent o]

  mapM_ print approxJobAlgDets

qAlgorithmByName :: String -> QAlgorithms.QueueAlgorithm
qAlgorithmByName name = case name of
  "so" -> QAlgorithms.so
  "lo" -> QAlgorithms.lo
  "fifo" -> QAlgorithms.fifo
  "lifo" -> QAlgorithms.lifo
  "sjlo" -> QAlgorithms.sjlo
  "sjso" -> QAlgorithms.sjso
  "ljso" -> QAlgorithms.ljso
  "ljlo" -> QAlgorithms.ljlo
  "rrso" -> QAlgorithms.rrso
  "rrlo" -> QAlgorithms.rrlo
  "sjmd" -> QAlgorithms.sjmd
  "sjmdr" -> QAlgorithms.sjmdr
  "md" -> QAlgorithms.md
  "mdr" -> QAlgorithms.mdr
  _ -> undefined
