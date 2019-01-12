module ExploratoryTests
  ( exploratoryTests
  ) where

import Data.List.Extra (nubOrd)
import qualified Data.Heap

import Test.Tasty
import Test.Tasty.HUnit

exploratoryTests :: TestTree
exploratoryTests = testGroup "Exploratory tests" [dummyTest
                                                 , nubOrdTest
                                                 , emptyHeapTest
                                                 , filledHeapTest
                                                 , heapManagementTest
                                                 ]
dummyTest :: TestTree
dummyTest = testCase "Test nothing"
  (assertEqual "" True True)

nubOrdTest :: TestTree
nubOrdTest = testCase "Test nubOrd"
  (assertEqual "" [1,2,3] (nubOrd [1,2,1,2,3,3,3,3]))

emptyHeapTest :: TestTree
emptyHeapTest = testCase "Test heap empty"
  (assertEqual "" True (Data.Heap.isEmpty heap))
  where heap = Data.Heap.empty

filledHeapTest :: TestTree
filledHeapTest = testCase "Test heap not empty"
  (assertEqual "" False (Data.Heap.isEmpty heap))
  where heap = Data.Heap.fromList [1,2,3,4,5] :: Data.Heap.MinHeap Int

heapManagementTest :: TestTree
heapManagementTest = testCase "Test heap operations"
  (assertEqual "" 0 (item))
  where item = case popResult of
          Just (item', _) -> item'
          Nothing -> -1
        popResult = Data.Heap.view heap'
        heap' = Data.Heap.insert 0 heap
        heap = Data.Heap.fromList [3,2,1,4,5] :: Data.Heap.MinHeap Int
