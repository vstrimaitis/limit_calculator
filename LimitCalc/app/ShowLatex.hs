module ShowLatex
    ( ShowLatex
    , showLatex
    ) where
    
import LimitCalc.Ast
import LimitCalc.Point
import qualified LimitCalc.AstPoint as P

parens :: Integer -> Integer -> String -> String
parens a b val
    | a < b = "(" ++ val ++ ")"
    | otherwise = val

class ShowLatex a where
    showLatex :: a -> String

instance ShowLatex Double where
    showLatex 0 = "0"
    showLatex x = fixString (show x)
        where
            fixString s
                | endsWithZero s = take (length s - 2) s
                | otherwise = s

            endsWithZero ".0" = True
            endsWithZero (_:xs) = endsWithZero xs
            endsWithZero _ = False
        
instance ShowLatex a => ShowLatex (Expr a) where
    showLatex expr = go 0 expr
        where
            go _ (Const a) = showLatex a
            go _ X = "x"
            go _ Pi = "\\pi"
            go _ E = "e"
            go _ (Negate e) = "-" ++ go 4 e
            go p (BinaryOp Add a b) = parens 1 p $ go 1 a ++ " + " ++ go 2 b
            go p (BinaryOp Subtract a b) = parens 1 p $ go 1 a ++ " - " ++ go 2 b
            go p (BinaryOp Multiply a b) = parens 2 p $ go 1 a ++ " \\cdotp " ++ go 2 b
            go p (BinaryOp Divide a b) = "\\frac{" ++ go 0 a ++ "}{" ++ go 0 b ++ "}"
            go p (BinaryOp Power a b) = parens 4 p $ go 6 a ++ " ^{" ++ go 0 b ++ "}"
            go p (Function Sin a) = parens 5 p $ "sin(" ++ go 0 a ++ ")"
            go p (Function Cos a) = parens 5 p $ "cos(" ++ go 0 a ++ ")"
            go p (Function Atan a) = parens 5 p $ "arctg(" ++ go 0 a ++ ")"
            go p (Function Exp a) = parens 5 p $ "exp(" ++ go 0 a ++ ")"
            go p (Function Ln a) = parens 5 p $ "ln(" ++ go 0 a ++ ")"
            go p (Function Sqrt a) = parens 5 p $ "\\sqrt{" ++ go 0 a ++ "}"

instance ShowLatex a => ShowLatex (Point a) where
    showLatex PositiveInfinity = "+\\infty"
    showLatex NegativeInfinity = "-\\infty"
    showLatex (Finite val) = showLatex val

instance ShowLatex a => ShowLatex (P.Value a) where
    showLatex val = go 0 val
        where
            go _ (P.Const a) = showLatex a
            go _ P.Pi = "\\pi"
            go _ P.E = "e"
            go _ (P.Negate e) = "-" ++ go 4 e
            go p (P.BinaryOp Add a b) = parens 1 p $ go 1 a ++ " + " ++ go 2 b
            go p (P.BinaryOp Subtract a b) = parens 1 p $ go 1 a ++ " - " ++ go 2 b
            go p (P.BinaryOp Multiply a b) = parens 2 p $ go 1 a ++ " \\cdotp " ++ go 2 b
            go p (P.BinaryOp Divide a b) = "\\frac{" ++ go 0 a ++ "}{" ++ go 0 b ++ "}"
            go p (P.BinaryOp Power a b) = parens 4 p $ go 5 a ++ " ^{" ++ go 0 b ++ "}"
            