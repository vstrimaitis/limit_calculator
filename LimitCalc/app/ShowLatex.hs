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
    showLatex x = fixString (show x)
        where
            fixString s
                | endsWithZero s = take (length s - 2) s
                | otherwise = s

            endsWithZero ".0" = True
            endsWithZero (_:xs) = endsWithZero xs
        
instance ShowLatex a => ShowLatex (Expr a) where
    showLatex expr = go 0 expr
        where
            go _ (Const a) = showLatex a
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
            go _ (P.Negate e) = go 4 e
            go p (P.BinaryOp Add a b) = parens 1 p $ go 1 a ++ " + " ++ go 2 b
            go p (P.BinaryOp Subtract a b) = parens 1 p $ go 1 a ++ " - " ++ go 2 b
            go p (P.BinaryOp Multiply a b) = parens 2 p $ go 1 a ++ " \\cdotp " ++ go 2 b
            go p (P.BinaryOp Divide a b) = "\\frac{" ++ go 0 a ++ "}{" ++ go 0 b ++ "}"
            go p (P.BinaryOp Power a b) = parens 4 p $ go 5 a ++ " ^{" ++ go 0 b ++ "}"
            