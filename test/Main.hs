module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Utils
import Solution
import Job
import Operation
import Machine
import Assignment
  
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
                                     , mStretchTest
                                     , tStretchTest
                                     , wStretchTest
                                     , pStretchTest
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
    result = totalFlow jobs assignments

costsTest :: TestTree
costsTest = testCase "Calculate some costs"
  (assertEqual "" [(1, jobs !! 0), (1, jobs !! 1)] (costs jobs assignments cost))
  where
    cost j _ = 1

mStretchTest :: TestTree
mStretchTest = testCase "Calculate m-stretch"
  (assertEqual "" (6.0/4.0) (mStretch job' assignments'))
  where
    job' = Job 1 0 0
    operations' = [Operation 1 1 0 0 2 0, Operation 1 2 0 0 4 0]
    machine' = Machine 1 0
    assignments' = [Assignment 2 (operations' !! 0) machine', Assignment 6 (operations' !! 1) machine']

tStretchTest :: TestTree
tStretchTest = testCase "Calculate t-stretch"
  (assertEqual "" (6.0/6.0) (tStretch job' assignments'))
  where
    job' = Job 1 0 0
    operations' = [Operation 1 1 0 0 2 0, Operation 1 2 0 0 4 0]
    machine' = Machine 1 0
    assignments' = [Assignment 2 (operations' !! 0) machine', Assignment 6 (operations' !! 1) machine']

wStretchTest :: TestTree
wStretchTest = testCase "Calculate w-stretch"
  (assertEqual "" (6.0/(6.0/2.0)) (wStretch machines' job' assignments'))
  where
    job' = Job 1 0 0
    operations' = [Operation 1 1 0 0 2 0, Operation 1 2 0 0 4 0]
    machines' = [Machine 1 0, Machine 2 0]
    assignments' = [Assignment 2 (operations' !! 0) (machines' !! 0)
                   , Assignment 6 (operations' !! 1) (machines' !! 0)]

pStretchTest :: TestTree
pStretchTest = testCase "Calculate p-stretch"
  (assertEqual "" (6.0/4.0) (pStretch fopts job' assignments'))
  where
    job' = Job 1 0 0
    operations' = [Operation 1 1 0 0 2 0, Operation 1 2 0 0 4 0]
    machines' = [Machine 1 0, Machine 2 0]
    assignments' = [Assignment 2 (operations' !! 0) (machines' !! 0)
                   , Assignment 6 (operations' !! 1) (machines' !! 0)]
    fopts = [(4, job')]
