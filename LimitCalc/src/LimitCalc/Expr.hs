{-# LANGUAGE DeriveFunctor #-}

module LimitCalc.Expr
    ( Expr(..)
    , Op(..)
    , Fn(..)
    , negate
    , squareRoot
    , power
    , substituteX
    ) where
    
import Prelude hiding (negate)
import Data.Ratio ((%), numerator, denominator)
import LimitCalc.Limits

data Expr a
    = Const a
    | X
    | Pi
    | BinaryOp Op (Expr a) (Expr a)
    | IntegerPower (Expr a) Integer
    | Function Fn (Expr a)
    deriving (Show, Functor)

data Op
    = Add
    | Subtract
    | Multiply
    | Divide
    deriving Show

data Fn
    = Sin
    | Cos
    | Atan
    | Exp
    | Ln
    deriving Show

negate :: Num a => Expr a -> Expr a
negate (Const x) = Const (-x)
negate expr = BinaryOp Multiply (Const $ -1) expr

squareRoot :: Fractional a => Expr a -> Expr a
squareRoot e = power e $ Const $ fromRational (1 % 2)

power :: Fractional a => Expr a -> Expr Rational -> Expr a
power a b = case b of
    Const x | denominator x == 1 ->
        if x < 0 then
            BinaryOp Divide (Const 1) (IntegerPower a (-numerator x))
        else if x == 0 then
            Const 1
        else
            IntegerPower a (numerator x)
    _ -> Function Exp (BinaryOp Multiply (fmap fromRational b) (Function Ln a))

substituteX :: Expr a -> Expr a -> Expr a
substituteX with X = with
substituteX _ e@(Const _) = e
substituteX _ Pi = Pi
substituteX with (BinaryOp op a b) = BinaryOp op (substituteX with a) (substituteX with b)
substituteX with (Function fn a) = Function fn (substituteX with a)
substituteX with (IntegerPower a n) = IntegerPower (substituteX with a) n
