module Lib
    ( Point(..)
    , Limit(..)
    , findLimit
    , check
    ) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Expr
import Series
import Parsing
import Limits

check :: (Eq a, Ord a, Floating a, Show a) => a -> String -> String
check at = either show (show . findLimit (Finite at)) . parseExpr

findLimit :: (Eq a, Ord a, Floating a) => Point a -> Expr a -> Limit a
findLimit (Finite x) = limitAtZero . substituteX (BinaryOp Add (Const x) X)
findLimit PositiveInfinity = limitAtZero . substituteX (overXSquared 1)
findLimit NegativeInfinity = limitAtZero . substituteX (overXSquared (-1))

overXSquared :: Num a => a -> Expr a
overXSquared c = BinaryOp Divide (Const c) (BinaryOp Multiply X X)

limitAtZero :: (Eq a, Ord a, Floating a) => Expr a -> Limit a
limitAtZero expr = fromMaybe fromPositive fromNegative
    where
        (pos, neg) = seriesToCoefs series
        series = fromNum 1--foldExpr expr

        rawFromNegative [] _ = Nothing
        rawFromNegative (x:xs) (lim:ys)
            | x == 0 = rawFromNegative xs ys
            | x > 0 = rawFromNegative xs ys <|> Just lim
            | otherwise = rawFromNegative xs ys <|> Just (flipSign lim)

        fromNegative = rawFromNegative neg $ cycle [NoLimit, HasLimit PositiveInfinity]

        fromPositive = HasLimit $ Finite $
            case pos of
                []    -> 0
                (x:_) -> x

        flipSign (HasLimit PositiveInfinity) = HasLimit NegativeInfinity
        flipSign (HasLimit NegativeInfinity) = HasLimit PositiveInfinity
        flipSign other = other


