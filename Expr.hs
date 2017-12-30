module Expr where

    infixl 3 :-
    infixl 4 :+:, :-:
    infixl 5 :*:, :/:
    infixl 6 :^:

    data Expr a
        = X
        | Const a
        | (:-) (Expr a)
        | (Expr a) :+: (Expr a)
        | (Expr a) :-: (Expr a)
        | (Expr a) :*: (Expr a)
        | (Expr a) :/: (Expr a)
        | (Expr a) :^: (Expr a)
        | Log (Expr a)
        | Sin (Expr a)
        | Cos (Expr a)
        | E
        deriving (Eq)

    instance Show a => Show (Expr a) where
        show X = "x"
        show (Const x) = show x
        show ((:-) x) = "(-" ++ show x ++ ")"
        show (x :+: y) = "(" ++ show x ++ " + " ++ show y ++ ")"
        show (x :-: y) = "(" ++ show x ++ " - " ++ show y ++ ")"
        show (x :*: y) = "(" ++ show x ++ " * " ++ show y ++ ")"
        show (x :/: y) = "(" ++ show x ++ " / " ++ show y ++ ")"
        show (x :^: y) = "(" ++ show x ++ " ^ " ++ show y ++ ")"
        show (Log x) = "ln(" ++ show x ++ ")"
        show (Sin x) = "sin(" ++ show x ++ ")"
        show (Cos x) = "cos(" ++ show x ++ ")"
        show E = "e"
    