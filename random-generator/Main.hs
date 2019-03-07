{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (unfoldr)
import qualified System.Random

import Options.Generic (ParseRecord, Generic, getRecord)

data Arguments
  = Simple { jobs :: Int
           , jopslb :: Int
           , jopsub :: Int
           , oplenlb :: Int
           , oplenub :: Int
           , nextarrlb :: Int
           , nextarrub :: Int
           }
  deriving (Generic, Show)

instance ParseRecord Arguments

main :: IO ()
main = do
  parsedArgs <- getRecord "Random instance generator"
  main' parsedArgs

main' :: Arguments -> IO ()
main' (Simple js jopsl jopsu opll oplu nal nau) = do
  g <- System.Random.getStdGen
  let rs = System.Random.randomRs (0, maxBound) g :: [Int]
  let (jOpNums, rs') = (map (narrow (jopsl, jopsu)) $ take js rs, drop js rs)
  let (intervals, rs'') = (map (narrow (nal, nau)) $ take (js-1) rs', drop (js-1) rs')
  let jOpLenss = unfoldr generate (jOpNums, rs'')
        where generate (jOpNums', rz)
                | jOpNums' == [] = Nothing
                | otherwise = Just (map (narrow (opll, oplu)) $ take (head jOpNums') rz
                                   , (tail jOpNums', drop (head jOpNums') rz))
  print jOpLenss
  print intervals
  -- TODO: assemble

narrow :: (Int, Int) -> Int -> Int
narrow (lb, ub) x = lb + (x `mod` (ub - lb + 1))
