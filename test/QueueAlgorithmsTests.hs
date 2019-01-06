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
assignInTimeFrameTest'' = testCase "Test algorithm assigning to empty machine"
  (assertEqual "" (expectedMops, expectedQ, expectedAs) (assignInTimeFrame [emptyMachine'] queue' 0 1))
  where
    expectedMops = [(machine', Just (operation', 2))]
    expectedQ = []
    expectedAs = []
    emptyMachine' = (machine', Nothing)
    machine' = Machine 1 0
    operation' = Operation 1 1 0 0 2 0
    queue' = [operation']
