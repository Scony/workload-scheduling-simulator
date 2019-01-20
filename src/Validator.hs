module Validator
  ( validateSolution
  ) where

import Data.List (sortBy)
import Data.List.Extra (nubOrdBy)
import qualified Data.IntMap.Strict as Map

import Assignment (Assignment, finish, operation, machine)
import Job (Job, arrival)
import Operation (Operation, duration, parentOf, uuid)
import Machine (uuid)
import Utils (assert)

validateSolution :: [Job] -> [Operation] -> [Assignment] -> [Assignment]
validateSolution js ops as = validateSolutionSize ops
                             $ validateEachMachineProcessAtMostOneOperationAtTheTime
                             $ validateSingularOperationExecutions ops
                             $ validateOperationFinishes js as

validateSolutionSize :: [Operation] -> [Assignment] -> [Assignment]
validateSolutionSize ops as = assert (length ops == length as) msg as
  where msg = "Solution size is invalid"

validateOperationFinishes :: [Job] -> [Assignment] -> [Assignment]
validateOperationFinishes js as = assert (all operationFinishIsValid as) msg as
  where msg = "Operation finishes are not valid"
        operationFinishIsValid a = finish a
                                   >=
                                   arrival (parentOf js $ operation a) + duration (operation a)

validateSingularOperationExecutions :: [Operation] -> [Assignment] -> [Assignment]
validateSingularOperationExecutions ops as = assert (length ops == length uniqueAs) msg as
  where msg = "Operation executions are not singular: "
        uniqueAs = nubOrdBy cmp as
        cmp a b = compare (Operation.uuid $ operation a) (Operation.uuid $ operation b)

validateEachMachineProcessAtMostOneOperationAtTheTime :: [Assignment] -> [Assignment]
validateEachMachineProcessAtMostOneOperationAtTheTime as = assert allMsValid msg as
  where msg = "Some machine process more than one operation at the time"
        allMsValid = all validateAs sortedAss
        validateAs as' = all pairIsFine $ zip as' $ tail as'
        pairIsFine (l, r) = finish l <= beginR
          where beginR = finish r - duration (operation r)
        sortedAss = map (sortBy cmp) ass
        cmp l r = if beginL == beginR then compare (finish l) (finish r) else compare beginL beginR
          where beginL = finish l - duration (operation l)
                beginR = finish r - duration (operation r)
        ass = map snd
              $ Map.toList
              $ foldl mapInserter Map.empty as
        mapInserter acc a = Map.insertWith (\_ as' -> a:as') (Machine.uuid $ machine a) [a] acc
