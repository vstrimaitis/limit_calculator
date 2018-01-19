module LimitCalc.Exact(Exact(..)) where

import Data.Ratio
import LimitCalc.Sign

data Exact
    = Rat Rational
    | Sin Exact
    | Cos Exact
    | Atan Exact
    | Ln Exact
    | Exp Exact
    | Add Exact Exact
    | Negate Exact
    | Mul Exact Exact
    | Div Exact Exact
    | Pi
    | E
    deriving (Show, Eq)

simplifyStep :: Exact -> Maybe Exact
simplifyStep (Atan 0) = Just 0
simplifyStep (Sin 0) = Just 0
simplifyStep (Sin Pi) = Just 0
simplifyStep (Cos Pi) = Just (-1)
simplifyStep (Cos 0) = Just 1
simplifyStep (Ln 1) = Just 0
simplifyStep (Exp 0) = Just 1
simplifyStep (Exp 1) = Just E
simplifyStep (Exp (Ln x)) = Just x
simplifyStep (Ln (Exp x)) = Just x
simplifyStep (Negate (Rat a)) = Just $ Rat $ negate a
simplifyStep (Add (Rat a) (Rat b)) = Just $ Rat $ a + b
simplifyStep (Mul (Rat a) (Rat b)) = Just $ Rat $ a * b
simplifyStep (Div (Rat a) (Rat b)) = Just $ Rat $ a / b
simplifyStep (Add 0 x) = Just x
simplifyStep (Add x 0) = Just x
simplifyStep (Mul 1 x) = Just x
simplifyStep (Mul x 1) = Just x
simplifyStep (Div 0 _) = Just 0
simplifyStep (Div x 1) = Just x
simplifyStep e@(Add a (Negate b))
    | a == b = Just 0
    | otherwise = Nothing
simplifyStep e@(Add (Negate a) b)
    | a == b = Just 0
    | otherwise = Nothing
simplifyStep other = Nothing

simplify :: Exact -> Exact
simplify e = case simplifyStep e of
    Just e' -> simplify e'
    Nothing -> e

instance Num Exact where
    a + b = simplify $ Add a b
    a * b = simplify $ Mul a b
    abs = error "no abs for exact"
    signum = error "no signum for exact"
    fromInteger = Rat . fromInteger
    negate = simplify . Negate

instance Fractional Exact where
    fromRational = Rat
    a / b = simplify $ Div a b

instance Floating Exact where
    pi = Pi
    exp = simplify . Exp
    log = simplify . Ln
    sin = simplify . Sin
    cos = simplify . Cos
    asin = error "no asin for exact"
    acos = error "no acos for exact"
    atan = Atan
    sinh = error "no sinh for exact"
    cosh = error "no cosh for exact"
    asinh = error "no asinh for exact"
    acosh = error "no acosh for exact"
    atanh = error "no atanh for exact"

instance MaybeSigned Exact where
    getSign (Rat x)
        | x < 0 = Just Negative
        | x > 0 = Just Positive
        | otherwise = Just Zero
    getSign (Exp _) = Just Positive
    getSign (Atan x) = getSign x
    getSign Pi = Just Positive
    getSign E = Just Positive
    getSign x = Nothing
