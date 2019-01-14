module Utils
  ( slice
  , assert
  ) where

slice a b = take (b - a) . drop a

assert pred msg x = if pred then x else error msg
