{-# LANGUAGE DeriveFunctor #-}

module LimitCalc.Expr
    ( Expr(..)
    , Op(..)
    , Fn(..)
    , negate
    , power
    , substituteX
    , fromAst
    ) where
    
import Prelude hiding (negate)
import Data.Ratio ((%), numerator, denominator)
import qualified LimitCalc.Ast as Ast

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

fromAst :: Fractional a => Ast.Expr Rational -> Expr a
fromAst (Ast.Const x) = Const $ fromRational x
fromAst Ast.X = X
fromAst Ast.Pi = Pi
fromAst Ast.E = Function Exp (Const 1)
fromAst (Ast.Negate a) = negate (fromAst a)
fromAst (Ast.BinaryOp Ast.Add a b) = BinaryOp Add (fromAst a) (fromAst b)
fromAst (Ast.BinaryOp Ast.Subtract a b) = BinaryOp Subtract (fromAst a) (fromAst b)
fromAst (Ast.BinaryOp Ast.Multiply a b) = BinaryOp Multiply (fromAst a) (fromAst b)
fromAst (Ast.BinaryOp Ast.Divide a b) = BinaryOp Divide (fromAst a) (fromAst b)
fromAst (Ast.BinaryOp Ast.Power a b) = power (fromAst a) (fromAst b)
fromAst (Ast.Function Ast.Sin a) = Function Sin (fromAst a)
fromAst (Ast.Function Ast.Cos a) = Function Cos (fromAst a)
fromAst (Ast.Function Ast.Atan a) = Function Atan (fromAst a)
fromAst (Ast.Function Ast.Exp a) = Function Exp (fromAst a)
fromAst (Ast.Function Ast.Ln a) = Function Ln (fromAst a)
fromAst (Ast.Function Ast.Sqrt a) = power (fromAst a) (Const $ 1 % 2)
