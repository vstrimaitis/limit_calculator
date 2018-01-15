{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module LimitCalc.Sign
    ( Sign(Negative, Zero, Positive)
    , MaybeSigned
    , getSign
    , isZero
    , isNonZero
    ) where

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

eps :: Double
eps = 1e-12

instance MaybeSigned Double where
    getSign x
        | x < -eps = Just Negative
        | x > eps = Just Positive
        | otherwise = Just Zero
