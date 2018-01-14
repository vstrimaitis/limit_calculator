{-# LANGUAGE ScopedTypeVariables #-}

module LimitCalc
    ( findLimit
    , check
    ) where

import LimitCalc.Expr
import LimitCalc.Parsing
import LimitCalc.Limits
import LimitCalc.Folding
import LimitCalc.Calc

check :: String -> String -> String
check at expr = either show show $ do
    e :: Expr Double <- parseExpr expr
    a <- parsePoint at
    return $ findLimit a e

findLimit :: (MaybeSigned a, Floating a) => Point a -> Expr a -> Limit a
findLimit point expr = case findLimit' point expr of
    Ok limit -> limit
    Undefined -> NoLimit
    MissingInfo -> Unknown

findLimit' :: (MaybeSigned a, Floating a) => Point a -> Expr a -> Calc (Limit a)
findLimit' (Finite x) = limitAtZero . substituteX (BinaryOp Add (Const x) X)
findLimit' PositiveInfinity = limitAtZero . substituteX (overXSquared 1)
findLimit' NegativeInfinity = limitAtZero . substituteX (overXSquared (-1))

overXSquared :: Num a => a -> Expr a
overXSquared c = BinaryOp Divide (Const c) (BinaryOp Multiply X X)
