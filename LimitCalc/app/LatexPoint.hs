module LatexPoint (makeLatex) where

import LimitCalc.Ast (Op(Add, Subtract, Multiply, Divide, Power))
import LimitCalc.AstPoint
import LimitCalc.Point
import LatexUtils

makeLatex :: Point (Value Double) -> String
makeLatex PositiveInfinity = "+\\infty"
makeLatex NegativeInfinity = "-\\infty"
makeLatex (Finite val) = go 0 val

go :: Integer -> Value Double -> String
go _ (Const a) = showDouble a
go _ Pi = "\\pi"
go _ E = "e"
go _ (Negate e) = go 4 e
go p (BinaryOp Add a b) = parens 1 p $ go 1 a ++ " + " ++ go 2 b
go p (BinaryOp Subtract a b) = parens 1 p $ go 1 a ++ " - " ++ go 2 b
go p (BinaryOp Multiply a b) = parens 2 p $ go 1 a ++ " \\cdotp " ++ go 2 b
go p (BinaryOp Divide a b) = "\\frac{" ++ go 0 a ++ "}{" ++ go 0 b ++ "}"
go p (BinaryOp Power a b) = parens 4 p $ go 5 a ++ " ^{" ++ go 0 b ++ "}"
