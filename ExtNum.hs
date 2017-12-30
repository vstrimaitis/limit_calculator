module ExtNum where

    data ExtNum a
        = R a
        | Inf
        | Ninf
        | Undefined
        deriving (Eq)

    instance Show a => Show (ExtNum a) where
        show (R a) = show a
        show Inf = "∞"
        show Ninf = "-∞"
        show Undefined = "undefined"

    instance Ord a => Ord (ExtNum a) where
        Undefined <= _ = False
        _ <= Undefined = False
        Ninf <= _    = True
        _    <= Inf  = True
        Inf  <= _    = False
        _    <= Ninf = False
        R a  <= R b  = a <= b

    instance (Num a, Ord a) => Num (ExtNum a) where
        Undefined + _ = Undefined
        _ + Undefined = Undefined
        R a + R b = R (a + b)
        Inf + Ninf = Undefined
        Inf + _ = Inf
        Ninf + Inf = Undefined
        Ninf + _ = Ninf

        Undefined * _ = Undefined
        _ * Undefined = Undefined
        R a * R b = R (a * b)
        Inf * Inf = Inf
        Inf * Ninf = Ninf
        Inf * (R a)
            | a < 0 =  Ninf
            | a == 0 = Undefined
            | otherwise = Inf
        Ninf * Inf = Ninf
        Ninf * Ninf = Inf
        Ninf * (R a)
            | a < 0 = Inf
            | a == 0 = Undefined
            | otherwise = Ninf
        (R a) * Inf = Inf * R a
        (R a) * Ninf = Ninf * R a

        negate Undefined = Undefined
        negate (R a) = R (negate a)
        negate Inf = Ninf
        negate Ninf = Inf

        abs Undefined = Undefined
        abs (R a) = R (abs a)
        abs _ = Inf

        signum Undefined = Undefined
        signum (R a) = R (signum a)
        signum Inf = 1
        signum Ninf = -1

        fromInteger a = R (fromInteger a)
        
    instance (Ord a, Fractional a) => Fractional (ExtNum a) where
        fromRational a = R (fromRational a)

        Undefined / _ = Undefined
        _ / Undefined = Undefined
        _ / R 0 = Undefined
        R a / R b = R (a / b)
        Inf / R a
            | a < 0 = Ninf
            | otherwise = Inf
        Inf / Inf = Undefined
        Inf / Ninf = Undefined
        Ninf / R a
            | a < 0 = Inf
            | otherwise = Ninf
        Ninf / Inf = Undefined
        Ninf / Ninf = Undefined
        R _ / _ = 0

    instance (Floating a, Ord a, RealFrac a) => Floating (ExtNum a) where
        Undefined ** _ = Undefined
        _ ** Undefined = Undefined
        R a ** R b = R (a ** b)
        R a ** Inf
            | a == 0 = Undefined
            | abs a < 1 = R 0
            | abs a > 1 = Inf
            | otherwise = Undefined
        R a ** Ninf
            | a == 0 = Undefined
            | abs a < 1 = Undefined
            | abs a > 1 = R 0
            | otherwise = Undefined
        Inf ** Inf = Inf
        Inf ** R _ = Inf
        Inf ** Ninf = R 0
        Ninf ** Inf = Undefined
        Ninf ** R a
            | a < 0 = R 1 / (Ninf ** R (-a))
            | a == 0 = Undefined
            | not (isInt a) = Undefined
            | even (round a) = Inf
            | otherwise = Ninf
        Ninf ** Ninf = R 0

        sqrt Undefined = Undefined
        sqrt (R a) = R (sqrt a)
        sqrt Inf = Inf
        sqrt Ninf = Undefined

        sin (R a) = R (sin a)
        sin _ = Undefined

        cos (R a) = R (cos a)
        cos _ = Undefined
            
        tan (R a)
            | isInt ((a-pi/2)/2) = Undefined
            | otherwise = R (tan a)
        tan _ = Undefined

        exp Undefined = Undefined
        exp (R a) = R (exp a)
        exp Inf = Inf
        exp Ninf = R 0

        log Undefined = Undefined
        log Inf = Inf
        log Ninf = Undefined
        log (R a)
            | a < 0 = Undefined
            | a == 0 = Ninf
            | otherwise = R (log a) 

        pi = R pi

        asin = undefined
        acos = undefined
        atan = undefined
        sinh = undefined
        cosh = undefined
        asinh = undefined
        acosh = undefined
        atanh = undefined










    
    isInt x = abs(x - fromInteger (round x)) < 1e-8