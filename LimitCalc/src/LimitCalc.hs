{-# LANGUAGE ScopedTypeVariables #-}

module LimitCalc
    ( Result(Unknown, Undefined, OutOfFuel, NoLimit, HasLimit)
    , findLimit
    , findLimitWithFuel
    ) where

import LimitCalc.Expr
import LimitCalc.Parsing
import LimitCalc.Point
import LimitCalc.Sign
import qualified LimitCalc.Limits as Limits
import LimitCalc.Folding
import LimitCalc.Calc hiding (Undefined)
import qualified LimitCalc.Calc as Calc

data Result a
    = Unknown
    | Undefined
    | OutOfFuel
    | NoLimit
    | HasLimit (Point a)
    deriving Show

defaultFuelAmount :: Integer
defaultFuelAmount = 100

findLimitWithFuel :: (MaybeSigned a, Floating a) => Integer -> Point a -> Expr a -> Result a
findLimitWithFuel fuel point expr = case runCalc (findLimit' point expr) fuel of
    Calc.Undefined -> Undefined
    MissingInfo -> Unknown
    Ok (Left _) -> OutOfFuel
    Ok (Right (_, Limits.Unknown)) -> Unknown
    Ok (Right (_, Limits.NoLimit)) -> NoLimit
    Ok (Right (_, Limits.HasLimit limit)) -> HasLimit limit

findLimit :: (MaybeSigned a, Floating a) => Point a -> Expr a -> Result a
findLimit = findLimitWithFuel defaultFuelAmount

findLimit' :: (MaybeSigned a, Floating a) => Point a -> Expr a -> Calc (Limits.Limit a)
findLimit' (Finite x) = limitAtZero . substituteX (BinaryOp Add (Const x) X)
findLimit' PositiveInfinity = limitAtZero . substituteX (overXSquared 1)
findLimit' NegativeInfinity = limitAtZero . substituteX (overXSquared (-1))

overXSquared :: Num a => a -> Expr a
overXSquared c = BinaryOp Divide (Const c) (BinaryOp Multiply X X)
