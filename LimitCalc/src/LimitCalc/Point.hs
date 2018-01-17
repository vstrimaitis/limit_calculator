{-# LANGUAGE DeriveFunctor #-}

module LimitCalc.Point where

data Point a = Finite a | PositiveInfinity | NegativeInfinity deriving Functor

instance Show a => Show (Point a) where
    show NegativeInfinity = "-inf"
    show PositiveInfinity = "+inf"
    show (Finite x) = show x
