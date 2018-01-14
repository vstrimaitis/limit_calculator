module LimitCalc.Limits where

data Point a = Finite a | PositiveInfinity | NegativeInfinity

data Limit a = HasLimit (Point a) | NoLimit | Unknown deriving Show

instance Show a => Show (Point a) where
    show NegativeInfinity = "-inf"
    show PositiveInfinity = "+inf"
    show (Finite x) = show x