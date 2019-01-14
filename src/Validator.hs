module Validator
  ( validateSolution
  ) where

import Data.List.Extra (nubOrdBy)

import Assignment (Assignment, finish, operation)
import Job (Job, arrival)
import Operation (Operation, duration, parentOf, uuid)
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
  where operationFinishIsValid a = finish a
                                   >=
                                   (arrival $ parentOf js $ operation a) + (duration $ operation a)
        msg = "Operation finishes are not valid"

validateSingularOperationExecutions :: [Operation] -> [Assignment] -> [Assignment]
validateSingularOperationExecutions ops as = assert (length ops == length uniqueAs) msg as
  where uniqueAs = nubOrdBy (\a b -> compare (uuid $ operation a) (uuid $ operation b)) as
        msg = "Operation executions are not singular: "

validateEachMachineProcessAtMostOneOperationAtTheTime :: [Assignment] -> [Assignment]
validateEachMachineProcessAtMostOneOperationAtTheTime as = assert True msg as
  where msg = ""
