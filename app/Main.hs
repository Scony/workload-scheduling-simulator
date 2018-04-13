module Main where

import System.Environment

import Input
import Algorithm
import Machine
import Solution

replace a b s = map (\x -> if x == a then b else x) s
mkLines x = map (replace '_' ' ') $ words $ replace ' ' '_' x

main :: IO ()
main = do
  stdin <- getContents
  args <- getArgs

  let machinesNum = read $ args !! 0 :: Int
  let machines = ordinaryMachines machinesNum
  putStrLn $ show machines

  let (jobs, operations) = (parseInstanceV2 . mkLines) stdin
  let jobsNum = length jobs
  let operationsNum = length operations

  putStrLn $ "machines: " ++ show machinesNum
  putStrLn $ "jobs: " ++ show jobsNum
  putStrLn $ "operations: " ++ show operationsNum

  putStrLn ""
  putStrLn $ show $ jobs
  putStrLn ""
  putStrLn $ show $ allInOne jobs operations machines
  putStrLn ""
  putStrLn $ show $ calculateAssignments jobs $ allInOne jobs operations machines
  putStrLn ""
  putStrLn $ show $ calculateJobFlows jobs $ calculateAssignments jobs $ allInOne jobs operations machines
  putStrLn ""
  putStrLn $ show $ calculateJobsTotalFlow $ calculateJobFlows jobs $ calculateAssignments jobs $ allInOne jobs operations machines

  putStrLn ""
  putStrLn $ show $ calculateJobsTotalFlow $ calculateJobFlows jobs $ calculateAssignments jobs $ opt jobs operations machines

  putStrLn ""
  putStrLn $ show $ calculateJobsTotalFlow $ calculateJobFlows jobs $ calculateAssignments jobs $ worst jobs operations machines
