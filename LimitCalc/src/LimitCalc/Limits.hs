module LimitCalc.Limits where

data Point a = Finite a | PositiveInfinity | NegativeInfinity deriving Show

data Limit a = HasLimit (Point a) | NoLimit | Unknown deriving Show
