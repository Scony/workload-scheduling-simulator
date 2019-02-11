module Utils
  ( slice
  , assert
  , mapJs2Ops, mapJs2Ops'
  ) where

import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map

import Job (Job, uuid)
import Operation (Operation, parent)

slice :: Int -> Int -> [a] -> [a]
slice a b = take (b - a) . drop a

assert :: Bool -> String -> a -> a
assert p msg x = if p then x else error msg

mapJs2Ops :: [Job] -> [Operation] -> Map.Map Job [Operation]
mapJs2Ops js ops = Map.fromList jOpss
  where jOpss = map (\(j, ops') -> (head [j' | j' <- js, Job.uuid j' == j], ops')) jOpss'
        jOpss' = IMap.toList $ mapJs2Ops' ops

mapJs2Ops' :: [Operation] -> IMap.IntMap [Operation]
mapJs2Ops' = foldl constructJOpsIMap IMap.empty
  where constructJOpsIMap acc o = IMap.insertWith (\_ ops' -> o:ops') (parent o) [o] acc
