module Lib
    ( Point(..)
    , Limit(..)
    , findLimit
    , check
    ) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Expr
import Prototype
import Parsing

data Point a = Finite a | PositiveInfinity | NegativeInfinity deriving Show

data Limit a = Known (Point a) | NoLimit | Unknown deriving Show

check :: (Eq a, Ord a, Floating a, Show a) => a -> String -> String
check at = either show (show . findLimit (Finite at)) . parseExpr

findLimit :: (Eq a, Ord a, Floating a) => Point a -> Expr a -> Limit a
findLimit (Finite x) = limitAtZero . substituteX (BinaryOp Add (Const x) X)
findLimit PositiveInfinity = limitAtZero . substituteX (overXSquared 1)
findLimit NegativeInfinity = limitAtZero . substituteX (overXSquared (-1))

overXSquared :: Num a => a -> Expr a
overXSquared c = BinaryOp Divide (Const c) (BinaryOp Multiply X X)

limitAtZero :: (Eq a, Ord a, Floating a) => Expr a -> Limit a
limitAtZero expr = fromMaybe (fromPositive $ sPos series) (fromNegative $ sNeg series)
    where
        series = foldExpr expr

        rawFromNegative [] _ = Nothing
        rawFromNegative (x:xs) (lim:ys)
            | x == 0 = rawFromNegative xs ys
            | x > 0 = rawFromNegative xs ys <|> Just lim
            | otherwise = rawFromNegative xs ys <|> Just (flipSign lim)

        fromNegative coefs = rawFromNegative coefs $ cycle [NoLimit, Known PositiveInfinity]

        fromPositive [] = Known $ Finite 0
        fromPositive (x:_) = Known $ Finite x

        flipSign (Known PositiveInfinity) = Known NegativeInfinity
        flipSign (Known NegativeInfinity) = Known PositiveInfinity
        flipSign other = other


