module LimitCalc.Derivative where

data Term a = Term {
    tCoef :: a,
    tExp  :: a
} deriving (Eq)

instance (Eq a, Num a, Show a) => Show (Term a) where
    show t
        | tCoef t == 0 = "0"
        | tExp t == 0 = show (tCoef t)
        | otherwise = show (tCoef t) ++ "*x^" ++ show (tExp t)

tMul :: (Num a, Eq a) => Term a -> Term a -> Term a
tMul a b = Term {tCoef = tCoef a * tCoef b, tExp = tExp a + tExp b}

tSame :: (Num a, Eq a) => Term a -> [Term a] -> [Term a]
tSame term = filter (\t -> tExp t == tExp term)

tDiff :: (Num a, Eq a) => Term a -> [Term a] -> [Term a]
tDiff term = filter (\t -> tExp t /= tExp term)

tEval :: Fractional a => Term Integer -> a -> a
tEval t a = c * a ^ tExp t
    where
        c = fromInteger $ tCoef t

dTerm :: (Num a, Eq a) => Term a -> Term a
dTerm t = Term {tCoef = c, tExp = e}
    where
        cc = tCoef t
        ee = tExp t
        c = cc * ee
        e = ee - 1




newtype Polynomial a = Poly [Term a]

instance (Eq a, Num a,Show a) => Show (Polynomial a) where
    show (Poly l) = join (map show l) " + "
        where
            join :: [String] -> String -> String
            join [] _ = ""
            join [x] _ = x
            join (x:xs) sep = x ++ sep ++ join xs sep

pCompress :: (Num a, Eq a) => Polynomial a -> Polynomial a
pCompress (Poly p) = pCompress' p []
    where
        pCompress' :: (Num a, Eq a) => [Term a] -> [Term a] -> Polynomial a
        pCompress' [] acc = Poly (filter (\t -> tCoef t /= 0) acc)
        pCompress' terms@(x:_) acc = pCompress' (tDiff x terms) (combine (tSame x terms) : acc) 

        combine :: (Num a, Eq a) => [Term a] -> Term a
        combine [] = Term {tCoef = 0, tExp = 0}
        combine terms = Term {tCoef = combo, tExp = tExp (head terms)}
            where
                combo = foldl (\acc x -> acc + tCoef x) 0 terms

pAdd :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
pAdd (Poly p1) (Poly p2) = pCompress (Poly (p1++p2))

pSub :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
pSub (Poly p1) (Poly p2) = pCompress (Poly (p1 ++ map (\t -> t {tCoef = -tCoef t}) p2))

pMul :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
pMul (Poly p1) (Poly p2) = pCompress (Poly [tMul x y | x <- p1, y <- p2])

pEval :: Fractional a => Polynomial Integer -> a -> a
pEval (Poly p) a = foldl (\acc t -> acc + tEval t a) 0 p

dPoly :: (Num a, Eq a) => Polynomial a -> Polynomial a
dPoly (Poly p) = pCompress $ Poly (map dTerm p)





data FracOpt a = FracOpt {
    fTop :: Polynomial a,
    fBottom :: Polynomial a,
    fBottomExp :: a
}

instance (Eq a, Num a, Show a) => Show (FracOpt a) where
    show f = "(" ++ show (fTop f) ++ ") / (" ++ show (fBottom f) ++ ") ^ " ++ show (fBottomExp f)

dFrac :: (Num a, Eq a) => FracOpt a -> FracOpt a
dFrac frac = FracOpt {fTop = t, fBottom = v, fBottomExp = n + 1}
    where
        u = fTop frac
        v = fBottom frac
        n = fBottomExp frac
        u' = dPoly u
        v' = dPoly v
        t = pSub (pMul u' v) (pMul (pMul (Poly [Term n 0]) u) v')


deri :: (Num a, Eq a) => Int -> FracOpt a -> FracOpt a
deri 0 = id
deri n = foldr1 (.) (replicate n dFrac)

fEval :: Fractional a => FracOpt Integer -> a -> a
fEval f a = pEval (fTop f) a / pEval (fBottom f) a ^ fBottomExp f

-- data Frac a = Frac {
--     fTop :: Polynomial a,
--     fBottom :: Polynomial a
-- }

-- instance (Eq a, Num a, Show a) => Show (Frac a) where
--     show f = "(" ++ show (fTop f) ++ ") / (" ++ show (fBottom f) ++ ")" 

-- dFrac :: (Num a, Eq a) => Frac a -> Frac a
-- dFrac frac = Frac {fTop = t, fBottom = b}
--     where
--         u = fTop frac
--         v = fBottom frac
--         u' = dPoly u
--         v' = dPoly v
--         t = pSub (pMul u' v) (pMul u v')
--         b = pMul v v


-- d :: (Num a, Eq a) => Int -> Frac a -> Frac a
-- d 0 = id
-- d n = foldr1 (.) (replicate n dFrac)

-- fEval :: (Floating a, Eq a) => Frac a -> a -> a
-- fEval f a = pEval (fTop f) a / pEval (fBottom f) a

-- atanFrac :: Frac Double
-- atanFrac = Frac (Poly [Term 1 0]) (Poly [Term 1 2, Term 1 0])
