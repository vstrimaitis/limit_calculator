{-# LANGUAGE DeriveFunctor #-}

module LimitCalc.Expr
    ( Expr(..)
    , Op(..)
    , Fn(..)
    , negate
    , squareRoot
    , substituteX
    , fixPowers
    ) where
    
import Prelude hiding (negate)
import Data.Ratio ((%))
import LimitCalc.Limits

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

squareRoot :: Fractional a => Expr a -> Expr a
squareRoot e = Power e (fromRational (1 % 2))

substituteX :: Expr a -> Expr a -> Expr a
substituteX with X = with
substituteX _ e@(Const _) = e
substituteX with (BinaryOp op a b) = BinaryOp op (substituteX with a) (substituteX with b)
substituteX with (Function fn a) = Function fn (substituteX with a)
substituteX with (Power a n) = Power (substituteX with a) n
substituteX with (IntegerPower a n) = IntegerPower (substituteX with a) n

fixPowers :: (MaybeSigned a, Num a) => Expr a -> Expr a
fixPowers (Const x) = Const x
fixPowers X = X
fixPowers (BinaryOp op a b) = BinaryOp op (fixPowers a) (fixPowers b)
fixPowers (Function fn a) = Function fn (fixPowers a)
fixPowers (IntegerPower x a)
    | a == 0 = Const 1
    | a < 0 = BinaryOp Divide (Const 1) (IntegerPower (fixPowers x) (-a))
    | otherwise = IntegerPower x a
fixPowers (Power x a) = case getSign a of
    Just Negative -> BinaryOp Divide (Const 1) (Power (fixPowers x) (-a))
    _ -> Power (fixPowers x) a
