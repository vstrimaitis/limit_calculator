module Lib
    ( Point(..)
    , Limit(..)
    , findLimit
    , check
    ) where

import Expr
import Parsing
import Limits
import Folding

check :: (Eq a, Ord a, Floating a, Show a) => a -> String -> String
check at = either show (show . findLimit (Finite at)) . parseExpr

findLimit :: (Eq a, Ord a, Floating a) => Point a -> Expr a -> Limit a
findLimit (Finite x) = limitAtZero . substituteX (BinaryOp Add (Const x) X) . fixPowers
findLimit PositiveInfinity = limitAtZero . substituteX (overXSquared 1) . fixPowers
findLimit NegativeInfinity = limitAtZero . substituteX (overXSquared (-1)) . fixPowers

overXSquared :: Num a => a -> Expr a
overXSquared c = BinaryOp Divide (Const c) (BinaryOp Multiply X X)
