{-# LANGUAGE DeriveFunctor #-}

module Expr
( Expr(..)
, Op(..)
, Fn(..)
, negate
) where
    
import Prelude hiding (negate)

data Expr a
    = Const a
    | X
    | BinaryOp Op (Expr a) (Expr a)
    | Function Fn (Expr a)
    deriving (Show, Functor)

data Op
    = Add
    | Subtract
    | Multiply
    | Divide
    | Power
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
