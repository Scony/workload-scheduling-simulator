module Main where

import Criterion.Main (defaultMain, bench, whnf)

import QueueAlgorithms
import Operation

main :: IO()
main = do
  defaultMain [ bench "fib 5" $ whnf fib 5
              , bench "sjlo 50.000" $ whnf sjlo $ operations 1000 50
              , bench "sjlo 100.000" $ whnf sjlo $ operations 2000 50
              , bench "sjlo 150.000" $ whnf sjlo $ operations 3000 50
              ]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

operations :: Int -> Int -> [Operation]
operations js opsPerJ = concat $ map operations' [1..js]
  where operations' j = [Operation j ((j-1)*opsPerJ+o) 0 0 o 0 | o <- [1..opsPerJ]]
