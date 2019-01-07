module QueueAlgorithmsTests
  ( queueAlgorithmsTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import QueueAlgorithms (assignInTimeFrame)
import Operation
import Machine
import Assignment

queueAlgorithmsTests :: TestTree
queueAlgorithmsTests = testGroup "QueueAlgotithms tests" [dummyTest
                                                         , assignInTimeFrameTest'
                                                         , assignInTimeFrameTest''
                                                         , assignInTimeFrameTest'''
                                                         , assignInTimeFrameTest''''
                                                         , assignInTimeFrameTest'''''
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
