{-# LANGUAGE ScopedTypeVariables #-}

module LimitCalc
    ( Result(Unknown, OutOfFuel, NoLimit, HasLimit)
    , findLimit
    , findLimitWithFuel
    , check
    ) where

import LimitCalc.Expr
import LimitCalc.Parsing
import LimitCalc.Limits hiding (NoLimit, Unknown, HasLimit)
import qualified LimitCalc.Limits as Limits
import LimitCalc.Folding
import LimitCalc.Calc

data Result a = Unknown | OutOfFuel | NoLimit | HasLimit (Point a) deriving Show

defaultFuelAmount :: Integer
defaultFuelAmount = 100

check :: String -> String -> String
check at expr = either show show $ do
    e :: Expr Double <- parseExpr expr
    a <- parsePoint at
    return $ findLimit a e

findLimitWithFuel :: (MaybeSigned a, Floating a) => Integer -> Point a -> Expr a -> Result a
findLimitWithFuel fuel point expr = case runCalc (findLimit' point expr) fuel of
    Undefined -> NoLimit
    MissingInfo -> Unknown
    Ok (Left _) -> OutOfFuel
    Ok (Right (_, Limits.Unknown)) -> Unknown
    Ok (Right (_, Limits.NoLimit)) -> NoLimit
    Ok (Right (_, Limits.HasLimit limit)) -> HasLimit limit

findLimit :: (MaybeSigned a, Floating a) => Point a -> Expr a -> Result a
findLimit = findLimitWithFuel defaultFuelAmount

findLimit' :: (MaybeSigned a, Floating a) => Point a -> Expr a -> Calc (Limit a)
findLimit' (Finite x) = limitAtZero . substituteX (BinaryOp Add (Const x) X)
findLimit' PositiveInfinity = limitAtZero . substituteX (overXSquared 1)
findLimit' NegativeInfinity = limitAtZero . substituteX (overXSquared (-1))

overXSquared :: Num a => a -> Expr a
overXSquared c = BinaryOp Divide (Const c) (BinaryOp Multiply X X)
