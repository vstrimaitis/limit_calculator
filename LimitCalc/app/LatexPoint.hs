module LatexPoint (makeLatex) where

import LimitCalc.Ast (Op(Add, Subtract, Multiply, Divide, Power))
import LimitCalc.AstPoint
import LimitCalc.Point

makeLatex :: Show a => Point (Value a) -> String
makeLatex PositiveInfinity = "+\\infty"
makeLatex NegativeInfinity = "-\\infty"
makeLatex (Finite val) = go 0 val

go :: Show a => Integer -> Value a -> String
go _ (Const a) = show a
go _ Pi = "\\pi"
go _ E = "e"
go _ (Negate e) = go 4 e
go p (BinaryOp Add a b) = parens 1 p $ go 1 a ++ " + " ++ go 2 b
go p (BinaryOp Subtract a b) = parens 1 p $ go 1 a ++ " - " ++ go 2 b
go p (BinaryOp Multiply a b) = parens 2 p $ go 1 a ++ " \\cdotp " ++ go 2 b
go p (BinaryOp Divide a b) = "\\frac{" ++ go 0 a ++ "}{" ++ go 0 b ++ "}"
go p (BinaryOp Power a b) = parens 4 p $ go 5 a ++ " ^{" ++ go 0 b ++ "}"

parens :: Integer -> Integer -> String -> String
parens a b val
    | a < b = "(" ++ val ++ ")"
    | otherwise = val
