module Utils
  ( slice
  ) where

slice a b = take (b - a) . drop a
