{-# LANGUAGE DeriveFunctor #-}

module LimitCalc.AstPoint where

import LimitCalc.Ast (Op(Add, Subtract, Multiply, Divide, Power))

data Value a
    = Const a
    | Pi
    | E
    | Negate (Value a)
    | BinaryOp Op (Value a) (Value a)
    deriving (Show, Functor)

foldToValue :: Floating a => Value a -> a
foldToValue (Const x) = x
foldToValue Pi = pi
foldToValue E = exp 1
foldToValue (Negate a) = -(foldToValue a)
foldToValue (BinaryOp Add a b) = (foldToValue a) + (foldToValue b)
foldToValue (BinaryOp Subtract a b) = (foldToValue a) - (foldToValue b)
foldToValue (BinaryOp Multiply a b) = (foldToValue a) * (foldToValue b)
foldToValue (BinaryOp Divide a b) = (foldToValue a) / (foldToValue b)
foldToValue (BinaryOp Power a b) = (foldToValue a) ** (foldToValue b)
