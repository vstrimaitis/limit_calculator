{-# LANGUAGE DeriveFunctor #-}

module Expr
    ( Expr(..)
    , Op(..)
    , Fn(..)
    , negate
    , substituteX
    , fixPowers
    ) where
    
import Prelude hiding (negate)

data Expr a
    = Const a
    | X
    | BinaryOp Op (Expr a) (Expr a)
    | Power (Expr a) a
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
negate = BinaryOp Multiply (Const $ -1)

substituteX :: Expr a -> Expr a -> Expr a
substituteX with X = with
substituteX with e@(Const _) = e
substituteX with (BinaryOp op a b) = BinaryOp op (substituteX with a) (substituteX with b)
substituteX with (Function fn a) = Function fn (substituteX with a)

fixPowers :: (Ord a, Num a) => Expr a -> Expr a
fixPowers (Const x) = Const x
fixPowers X = X
fixPowers (BinaryOp op a b) = BinaryOp op (fixPowers a) (fixPowers b)
fixPowers (Function fn a) = Function fn (fixPowers a)
fixPowers (IntegerPower x a)
    | a == 0 = Const 1
    | a < 0 = BinaryOp Divide (Const 1) (IntegerPower x (-a))
    | otherwise = IntegerPower x a
fixPowers (Power x a)
    | a < 0 = BinaryOp Divide (Const 1) (Power x (-a))
    | otherwise = Power x a
