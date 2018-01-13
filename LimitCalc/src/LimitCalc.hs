module LimitCalc
    ( findLimit
    , check
    ) where

import LimitCalc.Expr
import LimitCalc.Parsing
import LimitCalc.Limits
import LimitCalc.Folding
import LimitCalc.Calc

check :: (Ord a, MaybeSigned a, Floating a, Show a) => a -> String -> String
check at = either show (show . findLimit (Finite at)) . parseExpr

findLimit :: (Ord a, MaybeSigned a, Floating a) => Point a -> Expr a -> Calc (Limit a)
findLimit (Finite x) = limitAtZero . substituteX (BinaryOp Add (Const x) X) . fixPowers
findLimit PositiveInfinity = limitAtZero . substituteX (overXSquared 1) . fixPowers
findLimit NegativeInfinity = limitAtZero . substituteX (overXSquared (-1)) . fixPowers

overXSquared :: Num a => a -> Expr a
overXSquared c = BinaryOp Divide (Const c) (BinaryOp Multiply X X)
