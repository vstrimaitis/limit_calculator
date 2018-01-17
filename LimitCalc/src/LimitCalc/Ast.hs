{-# LANGUAGE DeriveFunctor #-}

module LimitCalc.Ast where

data Expr a
    = Const a
    | X
    | Pi
    | E
    | Negate (Expr a)
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
    | Sqrt
    deriving Show
