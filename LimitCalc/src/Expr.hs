{-# LANGUAGE DeriveFunctor #-}

module Expr
    ( Expr(..)
    , Op(..)
    , Fn(..)
    , negate
    , substituteX
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

substituteX :: Expr a -> Expr a -> Expr a
substituteX with X = with
substituteX with e@(Const _) = e
substituteX with (BinaryOp op a b) = BinaryOp op (substituteX with a) (substituteX with b)
substituteX with (Function fn a) = Function fn (substituteX with a)
