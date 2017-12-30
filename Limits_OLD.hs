module Limits where
    data Expr a
        = Const a
        | Sum (Expr a) (Expr a)
        | Prod (Expr a) (Expr a)
        | Exp (Expr a) (Expr a)
        | Log (Expr a)
        | Sin (Expr a)
        | Cos (Expr a)
        | Inv (Expr a)
        | Neg (Expr a)
        | E
        | Var String
        deriving (Eq)

    instance (Num a) => Num (Expr a) where
        (+) = Sum
        a - b = Sum a (Neg b)
        (*) = Prod
        negate = Neg
        signum = undefined
        abs = undefined
        fromInteger a = Const (fromInteger a)

    instance (Floating a) => Fractional (Expr a) where
        a / b = Prod a (Inv b)
        fromRational a = Const (fromRational a)

    instance (Floating a) => Floating (Expr a) where
        pi = Const pi
        exp = Exp E
        sqrt a = Exp a (1/2)
        log = Log
        sin = Sin
        cos = Cos
        tan a = Prod (Sin a) (Inv (Cos a))
        (**) = Exp
        logBase a b = Prod (Log a) (Inv (Log b))
        asin = undefined
        atan = undefined
        acos = undefined
        sinh = undefined
        cosh = undefined
        tanh = undefined
        asinh = undefined
        atanh = undefined
        acosh = undefined

    instance (Show a) => Show (Expr a) where
        show (Const a) = show a
        show (Var a) = a
        show (Prod a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
        show (Sum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
        show (Neg a) = '-' : show a
        show (Inv a) = '1' : '/' : show a
        show (Sin a) = "sin(" ++ show a ++ ")"
        show (Cos a) = "cos(" ++ show a ++ ")"
        show (Log a) = "ln(" ++ show a ++ ")"
        show (Exp a b) = "(" ++ show a ++ "^" ++ show b ++ ")"
        show E = "e"

    simp :: (Num a, Floating a, Eq a) => Expr a -> Expr a
    -- Constant distribution
    simp (Prod (Const a) (Prod (Const b) c)) = Prod (Const (a*b)) (simp c) -- A*(B*c) = (A*B)*c
    simp (Prod (Prod (Const a) c) (Const b)) = Prod (Const (a*b)) (simp c) -- (A*c)*B = (A*B)*c

    -- Logarithms
    simp (Log (Const 1)) = Const 0
    simp (Log E) = Const 1
    simp (Sum (Log a) (Log b)) = Log (Prod a b)
    simp (Sum (Log a) (Neg (Log b))) = Log (Prod a (Inv b))
    simp (Sum (Log a) (Neg (Neg (Log b)))) = Sum (Log a) (Log b)
    simp (Log (Exp a b)) = Prod b (Log a)

    -- Properties of sum
    simp (Sum (Const a) (Const b)) = Const (a+b) -- add constants
    simp (Sum (Const 0) a) = simp a -- 0 + a = a
    simp (Sum a (Const 0)) = simp a -- a + 0 = a
    simp (Sum a (Neg (Neg b))) = Sum a b
    simp (Sum (Neg (Neg b)) a) = Sum a b
    simp (Sum a (Neg b))
        | a == b = Const 0
        | b == 0 = simp a
        | otherwise = Sum (simp a) (Neg (simp b))
    simp (Sum (Neg b) a)
        | a == b = Const 0
        | b == 0 = simp a
        | otherwise = Sum (simp a) (Neg (simp b))
    simp (Sum a (Sum b c)) = Sum (Sum a b) c -- a+(b+c) = (a+b)+c

    -- Double negation
    simp (Neg (Neg a)) = a -- -(-a) = a

    -- Properties of multiplication
    simp (Prod (Const a) (Const b)) = Const (a*b) -- multiply constants
    simp (Prod (Const 0) _) = Const 0 -- 0 * a = 0
    simp (Prod _ (Const 0)) = Const 0 -- a * 0 = 0
    simp (Prod (Const 1) a) = simp a -- 1 * a = a
    simp (Prod a (Const 1)) = simp a -- a * 1 = a
    simp (Prod (Inv a) (Inv b)) = Inv (Prod a b) -- 1/a * 1/b = 1/(a*b)
    simp (Prod (Inv a) b) = Prod b (Inv a) -- 1/a*b = b*1/a
    simp (Prod (Prod a (Inv c)) (Inv b)) = Prod a (Inv (Prod b c)) -- (a/c)/b = a/(b*c)
    simp (Prod a (Prod b (Inv c))) = Prod (Prod a c) (Inv b) -- a/(b/c) = (a*c)/b
    simp (Prod a (Prod b c)) = Prod (Prod a b) c -- a*(b*c) = (a*b)*c
    simp (Prod (Neg a) (Neg b)) = Prod a b
    simp (Prod a (Neg b)) = Neg (Prod a b)
    simp (Prod (Neg b) a) = Neg (Prod a b)

    simp (Inv (Exp a n)) = Exp a (Neg n)

    -- Properties of exponentiation
    simp (Exp (Const a) (Const b)) = Const (a**b) -- raises constant to constant's power
    simp (Exp a (Const 1)) = simp a -- a^1 = a
    simp (Exp (Const 1) _) = Const 1 -- 1^a = 1
    simp (Exp _ (Const 0)) = Const 1 -- a^0 = 1
    simp (Exp (Const 0) _) = Const 0 -- 0^a = 0
    simp (Exp (Exp c (Const b)) (Const a)) = Exp c (Const (a*b)) -- (c^b)^a = c^(a*b)
    simp (Prod (Exp a m) (Exp b n))
        | a == b = Exp a (Sum n m) -- a^m * a^n = a^(m+n)
        | otherwise = Prod (Exp (simp a) (simp m)) (Exp (simp b) (simp n))
    simp (Exp (Prod a (Inv b)) (Neg n)) = Exp (Prod b (Inv a)) n -- (a/b)^(-n) = (b/a)^n
    simp (Exp (Prod a (Inv b)) n) = Prod (Exp a n) (Inv (Exp b n)) -- (a/b)^n = a^n / (b^n)
    simp (Exp (Prod a b) n) = Prod (Exp a n) (Exp b n)
    --simp (Exp a (Neg n)) = Inv (Exp a n)
    simp (Prod (Exp a n) (Inv (Exp b m)))
        | a == b = Exp a (Sum a (Neg m))
        | otherwise = Prod (Exp (simp a) (simp n)) (Inv (Exp (simp b) (simp m)))

    -- Distribution
    simp (Prod a (Sum b c)) = Sum (Prod a b) (Prod a c) -- a*(b+c) = a*b+a*c
    simp (Prod (Sum b c) a) = Sum (Prod a b) (Prod a c) -- (b+c)*a = a*b+a*c

    -- Fraction addition
    simp (Sum (Prod a (Inv b)) (Prod c (Inv d))) = Prod (Sum (Prod a d) (Prod b c)) (Inv (Prod b d)) -- a/b + c/d = (a*d+b*c)/(b*d)

    -- Trig
    simp (Sum (Exp (Sin a) (Const 2)) (Exp (Cos b) (Const 2))) 
        | a == b = Const 1 -- sin^2x + cos^2x = 1
        | otherwise = Sum (Exp (Sin (simp a)) (Const 2)) (Exp (Cos (simp b)) (Const 2))

    simp (Sin (Neg a)) = Neg (Sin a)
    simp (Cos (Neg a)) = Cos a
    simp (Sin (Sum a b)) = Sum (Prod (Sin a) (Cos b)) (Prod (Cos a) (Sin b))
    simp (Cos (Sum a b)) = Sum (Prod (Cos a) (Cos b)) (Neg (Prod (Sin a) (Sin b)))
    
    -- Base cases
    simp (Neg (Const 0)) = Const 0
    simp (Sum a b)
        | a == b = 2 * simp a
        | otherwise = simp a + simp b
    simp (Prod a b)
        | a == b = Exp (simp a) (Const 2)
        | otherwise = Prod (simp a) (simp b)
    simp (Exp a b) = Exp (simp a) (simp b)
    simp (Log a) = Log (simp a)
    simp (Var a) = Exp (Var a) (Const 1)
    simp a = a

    simplify :: (Num a, Floating a, Eq a) => Expr a -> Expr a
    simplify a = f a
        where
            b = simp a
            f a | b == a = a
                | otherwise = simplify b

                
    x = Var "x"
    y = Var "y"
    test = Prod (Sum (Var "a") (Var "b")) (Sum (Var "a") (Var "c"))
    test1 = Sum (Const 1.0) (Var "x")
    test2 = (Var "a" + Var "b") * 2.0 * 4.0
    test3 = Log 2.0
    test4 = sin (Exp (Var "x") 2 -Var "y") :: Expr Double
    test5 = x / x**2 :: Expr Double
    test6 = 1/(x**2) :: Expr Double
    test7 = x**(-2.0) :: Expr Double

    d :: (Floating a) => String -> Expr a -> Expr a
    d _ (Const _) = 0
    d x (Sum a b) = d x a + d x b
    d x (Prod a b) = (d x a * b) + (a * d x b)
    d x (Inv a) = - d x a / (a*a)
    d x (Neg a) = -(d x a)
    d x (Log a) = 1/a * d x a
    d x (Sin a) = cos a * d x a
    d x (Cos a) = -sin a * d x a
    d x (Exp E b) = exp b * d x b
    d x (Exp (Var a) (Const b))
        | a == x = Const b * Var a ** Const (b-1)
        | otherwise = 0
    d x (Exp a b) = (a ** b) * (d x a * b/a + d x b * log a)
    d x (Var y)
        | x == y = 1
        | otherwise = 0

    diff (Var x) a = simplify $ d x a



