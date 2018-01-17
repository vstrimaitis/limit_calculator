module LatexExpr (makeLatex) where

import LimitCalc.Ast
import LatexUtils

makeLatex :: Expr Double -> String
makeLatex expr = go 0 expr

go :: Integer -> Expr Double -> String
go _ (Const a) = showDouble a
go _ X = "x"
go _ Pi = "\\pi"
go _ E = "e"
go _ (Negate e) = go 4 e
go p (BinaryOp Add a b) = parens 1 p $ go 1 a ++ " + " ++ go 2 b
go p (BinaryOp Subtract a b) = parens 1 p $ go 1 a ++ " - " ++ go 2 b
go p (BinaryOp Multiply a b) = parens 2 p $ go 1 a ++ " \\cdotp " ++ go 2 b
go p (BinaryOp Divide a b) = "\\frac{" ++ go 0 a ++ "}{" ++ go 0 b ++ "}"
go p (BinaryOp Power a b) = parens 4 p $ go 5 a ++ " ^{" ++ go 0 b ++ "}"
go _ (Function Sin a) = "sin(" ++ go 0 a ++ ")"
go _ (Function Cos a) = "cos(" ++ go 0 a ++ ")"
go _ (Function Atan a) = "arctg(" ++ go 0 a ++ ")"
go _ (Function Exp a) = "exp(" ++ go 0 a ++ ")"
go _ (Function Ln a) = "ln(" ++ go 0 a ++ ")"
go _ (Function Sqrt a) = "\\sqrt{" ++ go 0 a ++ "}"
