module Assignment
  ( Assignment(Assignment, finish)
  ) where

import Operation
import Machine

data Assignment = Assignment { finish :: Int
                             , operation :: Operation
                             , machine :: Machine
                             } deriving (Show)

instance Eq Assignment where
  Assignment a _ _ == Assignment b _ _ = a == b
