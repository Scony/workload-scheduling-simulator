module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Utils (slice)
import Solution (calculateJobFlows, calculateJobsTotalFlow, costs)
import Job (Job (Job))
import Operation (Operation (Operation))
import Machine (Machine (Machine))
import Assignment (Assignment (Assignment))
  
jobs = [Job 1 0 0, Job 2 0 2]
operations = [Operation 1 1 0 0 2 0, Operation 2 2 0 0 2 0]
machine = Machine 1 0
assignments = [Assignment 2 (operations !! 0) machine, Assignment 4 (operations !! 1) machine]

main :: IO ()
main = do
  defaultMain (testGroup "Lib Tests" [dummyTest
                                     , sliceTest
                                     , sliceTest2
                                     , totalFlowTest
                                     , costsTest
                                     ])

dummyTest :: TestTree
dummyTest = testCase "Testing nothing"
  (assertEqual "Should pass" 10 (5 + 5))

sliceTest :: TestTree
sliceTest = testCase "Test slice"
  (assertEqual "Should return a slice" [4, 5] (slice 3 5 [1, 2, 3, 4, 5, 6, 7]))

sliceTest2 :: TestTree
sliceTest2 = testCase "Test slice2"
  (assertEqual "Should return a slice" [1, 2, 3, 4, 5, 6, 7] (slice 0 7 [1, 2, 3, 4, 5, 6, 7]))

totalFlowTest :: TestTree
totalFlowTest = testCase "Test total flow calculation"
  (assertEqual "Should return 4" 4 (result))
  where
    result = calculateJobsTotalFlow $ calculateJobFlows jobs assignments

costsTest :: TestTree
costsTest = testCase "Calculate some costs"
  (assertEqual "" [(1, jobs !! 0), (1, jobs !! 1)] (costs jobs assignments cost))
  where
    cost j _ = (1, j)
