{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hPutStrLn, stderr)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)

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
  = Online { algorithm :: String
           , machines :: Int
           , costfun :: String
           , restarts :: Bool
           , novalid :: Bool
           }                  -- running online algorithms
  | Offline String Int String -- running offline algorithms
  | Algdet String Int         -- approximated algorithm deteriorations
  deriving (Generic, Show)

instance ParseRecord Arguments

main :: IO ()
main = do
  parsedArgs <- getRecord "Multipurpose workload scheduling simulator"
  main' parsedArgs

main' :: Arguments -> IO ()

main' (Online algorithmName machinesNum costFunction restarts noValidation) = do
  stdin <- getContents

  let machines = ordinaryMachines machinesNum
  let (jobs, operations) = (parseInstanceV2 . mkLines) stdin
  let jobsNum = length jobs
  let operationsNum = length operations

  hPutStrLn stderr $ "> machines: " ++ show machinesNum
  hPutStrLn stderr $ "> jobs: " ++ show jobsNum
  hPutStrLn stderr $ "> operations: " ++ show operationsNum
  hPutStrLn stderr $ "> algorithm: " ++ algorithmName
  hPutStrLn stderr $ "> cost function: " ++ costFunction
  hPutStrLn stderr $ "> restarts: " ++ show restarts
  hPutStrLn stderr $ "> validation: " ++ show (not noValidation)

  let algorithm = qAlgorithmByName algorithmName
  let costFun a b = case costFunction of
        "flow" -> fromIntegral $ Solution.flow a b
        "mstretch" -> Solution.mStretch a b
        "tstretch" -> Solution.tStretch a b
        "wstretch" -> Solution.wStretch machines a b
        "apstretch" -> Solution.pStretch approxJobPerfectFlows a b
        _ -> error "choose cost function!!"
        where approxJobPerfectFlows = map approxJobPerfectFlow jobs
              approxJobPerfectFlow j = head $ Solution.costs [j] solution Solution.flow
                where solution = QAlgorithms.run runner' algorithm' [j] operations' machines
                      runner' = QAlgorithms.restartless
                      algorithm' = qAlgorithmByName "sjlo"
                      operations' = [o | o <- operations, uuid j == parent o]

  let runner = if restarts then QAlgorithms.restartful else QAlgorithms.restartless
  let validator js ops as = if noValidation then as else validateSolution js ops as
  let queueAlgorithm alg = Solution.costs jobs solution costFun
        where solution = validator jobs operations
                         $ QAlgorithms.run runner alg jobs operations machines
  let cjsInOrder = sortBy (\(_, j1) (_, j2) -> compare (arrival j1) (arrival j2))
  let jobCosts cjs = mapM_ (\(c, _) -> print c) (cjsInOrder cjs)

  jobCosts $ queueAlgorithm algorithm

main' (Offline algorithm machinesNum costFunction) = do
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
        _ -> error "choose cost function!!"

  let scheduleAlgorithm alg = Solution.costs jobs solution costFun
        where solution = validateSolution jobs operations
                         $ calculateSolution jobs
                         $ alg jobs operations machines
  let cjsInOrder = sortBy (\(_, j1) (_, j2) -> compare (arrival j1) (arrival j2))
  let jobCosts cjs = mapM_ (\(c, _) -> print c) (cjsInOrder cjs)

  case algorithm of
    "allin1" -> jobCosts $ scheduleAlgorithm OfflineAlgorithms.allInOne
    "opt" -> jobCosts $ scheduleAlgorithm (OfflineAlgorithms.opt Solution.totalFlow)
    "worst" -> jobCosts $ scheduleAlgorithm (OfflineAlgorithms.worst Solution.totalFlow)
    _ -> error "choose algorithm!!"

main' (Algdet algorithmName machinesNum) = do
  stdin <- getContents

  let machines = ordinaryMachines machinesNum
  let (jobs, operations) = (parseInstanceV2 . mkLines) stdin
  let jobsNum = length jobs
  let operationsNum = length operations

  hPutStrLn stderr $ "> machines: " ++ show machinesNum
  hPutStrLn stderr $ "> jobs: " ++ show jobsNum
  hPutStrLn stderr $ "> operations: " ++ show operationsNum
  hPutStrLn stderr $ "> algorithm: " ++ algorithmName

  let algorithm = qAlgorithmByName algorithmName
  let approxJobAlgDets = map approxJobAlgDet jobs
        where approxJobAlgDet j = fromIntegral (fst $ jobUnbiasedFlow j)
                                  / fromIntegral (fst $ approxJobPerfectFlow j)
              approxJobPerfectFlow j = head $ Solution.costs [j] solution Solution.flow
                where solution = QAlgorithms.run runner' algorithm' [j] operations' machines
                      algorithm' = qAlgorithmByName "sjlo"
                      operations' = [o | o <- operations, uuid j == parent o]
              jobUnbiasedFlow j = head $ Solution.costs [j] solution Solution.flow
                where solution = QAlgorithms.run runner' algorithm [j] operations' machines
                      operations' = [o | o <- operations, uuid j == parent o]
              runner' = QAlgorithms.restartless

  mapM_ print approxJobAlgDets

qAlgorithmByName :: String -> QAlgorithms.QueueAlgorithm
qAlgorithmByName name = fromMaybe (error "algorithm not found") (QAlgorithms.lookupByName name)
