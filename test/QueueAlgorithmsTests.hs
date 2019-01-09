module QueueAlgorithmsTests
  ( queueAlgorithmsTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import QueueAlgorithms (assignInTimeFrame, so)
import Operation
import Machine
import Assignment
import Job

queueAlgorithmsTests :: TestTree
queueAlgorithmsTests = testGroup "QueueAlgotithms tests" [dummyTest
                                                         , assignInTimeFrameTest'
                                                         , assignInTimeFrameTest''
                                                         , assignInTimeFrameTest'''
                                                         , assignInTimeFrameTest''''
                                                         , assignInTimeFrameTest'''''
                                                         , soTest
                                                         ]
dummyTest :: TestTree
dummyTest = testCase "Test nothing"
  (assertEqual "" True True)

assignInTimeFrameTest :: TestTree
assignInTimeFrameTest = testCase "Test algorithm assigning queue to machines"
  (assertEqual "" ([], [], []) (assignInTimeFrame [] [] 0 1))

assignInTimeFrameTest' :: TestTree
assignInTimeFrameTest' = testCase "Test algorithm making assignment from machine"
  (assertEqual "" (expectedMops, expectedQ, expectedAs) (assignInTimeFrame [filledMachine'] queue' 0 2))
  where
    expectedMops = [(machine', Nothing)]
    expectedQ = []
    expectedAs = [Assignment 2 operation' machine']
    filledMachine' = (machine', Just (operation', 2))
    machine' = Machine 1 0
    operation' = Operation 1 1 0 0 2 0
    queue' = []

assignInTimeFrameTest'' :: TestTree
assignInTimeFrameTest'' = testCase "Test algorithm filling empty machine"
  (assertEqual "" (expectedMops, expectedQ, expectedAs) (assignInTimeFrame [emptyMachine'] queue' 0 1))
  where
    expectedMops = [(machine', Just (operation', 2))]
    expectedQ = []
    expectedAs = []
    emptyMachine' = (machine', Nothing)
    machine' = Machine 1 0
    operation' = Operation 1 1 0 0 2 0
    queue' = [operation']

assignInTimeFrameTest''' :: TestTree
assignInTimeFrameTest''' = testCase "Test algorithm filling busy machine"
  (assertEqual "" (expectedMops, expectedQ, expectedAs) (assignInTimeFrame [filledMachine'] queue' 1 3))
  where
    expectedMops = [(machine', Just (operation'', 5))]
    expectedQ = []
    expectedAs = [Assignment 2 operation' machine']
    filledMachine' = (machine', Just (operation', 2))
    machine' = Machine 1 0
    operation' = Operation 1 1 0 0 2 0
    operation'' = Operation 1 2 0 0 3 0
    queue' = [operation'']

assignInTimeFrameTest'''' :: TestTree
assignInTimeFrameTest'''' = testCase "Test algorithm releasing but not filling"
  (assertEqual "" (expectedMops, expectedQ, expectedAs) (assignInTimeFrame [filledMachine'] queue' 1 2))
  where
    expectedMops = [(machine', Nothing)]
    expectedQ = [operation'']
    expectedAs = [Assignment 2 operation' machine']
    filledMachine' = (machine', Just (operation', 2))
    machine' = Machine 1 0
    operation' = Operation 1 1 0 0 2 0
    operation'' = Operation 1 2 0 0 3 0
    queue' = [operation'']

assignInTimeFrameTest''''' :: TestTree
assignInTimeFrameTest''''' = testCase "Test algorithm working for bigger instance"
  (assertEqual "" (expectedMops, expectedQ, expectedAs) (assignInTimeFrame mops' queue' 0 99))
  where
    expectedMops = mops'
    expectedQ = []
    expectedAs = [Assignment 2 (queue' !! 0) (machines' !! 0)
                 , Assignment 4 (queue' !! 2) (machines' !! 0)
                 , Assignment 3 (queue' !! 1) (machines' !! 1)
                 ]
    mops' = [(m, Nothing) | m <- machines']
    machines' = [Machine 1 0, Machine 2 0]
    queue' = [Operation 1 1 0 0 2 0, Operation 1 2 0 0 3 0, Operation 1 3 0 0 2 0]

soTest :: TestTree
soTest = testCase "Test algorithm working"
  (assertEqual "" expectedAs (so jobs' operations' machines'))
  where
    expectedAs = [
      Assignment 12 (operations' !! 2) (machines' !! 1)
      , Assignment 15 (operations' !! 0) (machines' !! 0)
      , Assignment 17 (operations' !! 1) (machines' !! 1)
      , Assignment 21 (operations' !! 3) (machines' !! 0)
      ]
    jobs' = [Job 1 0 0, Job 2 0 10, Job 3 0 20]
    operations' = [
      Operation 1 1 0 0 15 0
      , Operation 2 2 0 0 5 0
      , Operation 2 3 0 0 2 0
      , Operation 3 4 0 0 1 0
      ]
    machines' = [Machine 1 0, Machine 2 0]
