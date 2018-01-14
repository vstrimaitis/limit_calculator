{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module LimitCalc.Limits where

data Point a = Finite a | PositiveInfinity | NegativeInfinity

instance Show a => Show (Point a) where
    show NegativeInfinity = "-inf"
    show PositiveInfinity = "+inf"
    show (Finite x) = show x

data Limit a = HasLimit (Point a) | NoLimit | Unknown deriving Show

data Sign = Negative | Zero | Positive deriving Show

class MaybeSigned a where
    getSign :: a -> Maybe Sign
    isZero :: a -> Maybe Bool
    isZero x = case getSign x of
        Just Negative -> Just False
        Just Positive -> Just False
        Just Zero -> Just True
        Nothing -> Nothing
    isNonZero :: a -> Maybe Bool
    isNonZero x = case getSign x of
        Just Negative -> Just True
        Just Positive -> Just True
        Just Zero -> Just False
        Nothing -> Nothing

instance (Num a, Ord a) => MaybeSigned a where
    getSign x
        | x < 0 = Just Negative
        | x > 0 = Just Positive
        | otherwise = Just Zero
