module Prototype where

import Derivative
import Expr

data Series a = Series {
    sNeg :: [a],
    sPos :: [a]
}

fromNum :: (Num a) => a -> Series a
fromNum d = Series {sNeg = [], sPos = [d]}

x :: Num a => Series a
x = Series {sNeg = [], sPos = [0, 1]}

one :: Num a => Series a
one = fromNum 1

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

instance (Show a, Eq a, Num a) => Show (Series a) where
    show s = combine (showNeg (sNeg s)) (showPos (sPos s))
        where
            maxPosTerms = 10

            combine :: String -> String -> String
            combine "" "" = "0"
            combine "" s = s
            combine s "" = s
            combine s t = s ++ " + " ++ t

            showNeg :: (Show a, Eq a, Num a) => [a] -> String
            showNeg l = joinTerms (showNeg' (-1) l []) " + "

            showPos :: (Show a, Eq a, Num a) => [a] -> String
            showPos l = joinTerms (reverse (showPos' 0 l [])) " + "
            
            showNeg' :: (Show a, Eq a, Num a) => Int -> [a] -> [String] -> [String]
            showNeg' _ [] acc = filter (not . null) acc
            showNeg' n (x:xs) acc = showNeg' (n-1) xs (showTerm x n:acc)

            showPos' :: (Show a, Eq a, Num a) => Int -> [a] -> [String] -> [String]
            showPos' _ [] acc = filter (not . null) acc
            showPos' n (x:xs) acc
                | n >= maxPosTerms = filter (not . null) $ "..." : acc
                | otherwise = showPos' (n+1) xs (showTerm x n:acc)

            showTerm c n
                | c == 0 = ""
                | n == 0 = show c
                | otherwise = cc ++ "x" ++ e
                    where
                        cc
                            | c == 1 = ""
                            | otherwise = show c ++ "*"
                        e
                            | n == 1 = ""
                            | otherwise = "^" ++ show n

            joinTerms :: [String] -> String -> String
            joinTerms [] _ = ""
            joinTerms [x] _ = x
            joinTerms (x:xs) sep = x ++ sep ++ joinTerms xs sep

crazyZip :: Num a => (a -> a -> a) -> [a] -> [a] -> [a]
crazyZip _ [] [] = []
crazyZip f [] (y:ys) = crazyZip f [0] (y:ys)
crazyZip f (x:xs) [] = crazyZip f (x:xs) [0]
crazyZip f (x:xs) (y:ys) = f x y : crazyZip f xs ys

(+:) :: Num a => Series a -> Series a -> Series a
(+:) s1 s2 = Series { sNeg = crazyZip (+) (sNeg s1) (sNeg s2), sPos = crazyZip (+) (sPos s1) (sPos s2) }

(-:) :: Num a => Series a -> Series a -> Series a
(-:) s1 s2 = Series { sNeg = crazyZip (-) (sNeg s1) (sNeg s2), sPos = crazyZip (-) (sPos s1) (sPos s2) }

safeHead :: Num a => [a] -> a
safeHead [] = 0
safeHead (x:xs) = x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

mulBy :: Num a => Series a -> Int -> Series a
mulBy s 0 = s
mulBy s n
    | n < 0     = mulBy (Series {sNeg = safeHead (sPos s) : sNeg s, sPos = safeTail (sPos s)}) (n+1)
    | otherwise = mulBy (Series {sNeg = safeTail (sNeg s), sPos = safeHead (sNeg s) : sPos s}) (n-1) 

mul_ :: Num a => [a] -> [a] -> [a]
mul_ [] _ = []
mul_ _ [] = []
mul_ (x:xs) (y:ys) = (x*y) : crazyZip (+) d (0:c)
    where
        a = map (*x) ys
        b = map (*y) xs
        c = mul_ xs ys
        d = crazyZip (+) a b

(*:) :: Num a => Series a -> Series a -> Series a
(*:) s1 s2 = mulBy Series {sNeg = [], sPos = a} (-c1-c2)
    where
        a = mul_ b1 b2
        b1 = sPos s1'
        b2 = sPos s2'
        s1' = mulBy s1 c1
        s2' = mulBy s2 c2
        c1 = length (sNeg s1)
        c2 = length (sNeg s2)

div1 :: Fractional a => [a] -> [a] -> [a]
div1 _ [] = error "div by zero"
div1 [] (_:ys) = []
div1 xs@(x:_) ys@(y:_) = x/y : div1 (safeTail (crazyZip (-) xs (map (*(x/y)) ys ))) ys

div2 :: (Eq a, Fractional a) => [a] -> [a] -> Series a
div2 _ [] = error "im broken too"
div2 x (0:ys) = mulBy (div2 x ys) (-1)
div2 x y = Series {sNeg = [], sPos = div1 x y}

(/:) :: (Eq a, Fractional a) => Series a -> Series a -> Series a
(/:) s1 s2 = mulBy a (c2-c1)
    where
        a = div2 b1 b2
        b1 = sPos s1'
        b2 = sPos s2'
        s1' = mulBy s1 c1
        s2' = mulBy s2 c2
        c1 = length (sNeg s1)
        c2 = length (sNeg s2)

makeFunction :: Floating a => (Integer -> a -> a) -> Series a -> Series a
makeFunction deriv s 
    | (not . null) (sNeg s) = error "bad function arg"
    | otherwise = Series { sNeg = [], sPos = map getCoef [0..] }
        where
            getCoef n = (foldl1 (crazyZip (+)) (take (fromInteger n + 1) powers) !!! n) * coef n
            a = safeHead (sPos s)
            coefs = 0 : safeTail (sPos s)
            powers = iterate (mul_ coefs) [1]
            coef n = deriv n a

fsin :: Floating a => Series a -> Series a
fsin = makeFunction (\n a -> deriv n a / fromIntegral (fac n))
    where
        deriv n a
            | n `mod` 4 == 0 = sin a
            | n `mod` 4 == 1 = cos a
            | n `mod` 4 == 2 = -(sin a)
            | otherwise = -(cos a)
            
fcos :: Floating a => Series a -> Series a
fcos = makeFunction (\n a -> deriv n a / fromIntegral (fac n))
    where
        deriv n a
            | n `mod` 4 == 0 = cos a
            | n `mod` 4 == 1 = - (sin a)
            | n `mod` 4 == 2 = -(cos a)
            | otherwise = sin a

fe :: Floating a => Series a -> Series a
fe = makeFunction (\n a -> exp a / fromIntegral (fac n))

flog :: Floating a => Series a -> Series a
flog = makeFunction (\n a -> if n == 0 then log a else 1 / (fromInteger n * a ** fromInteger n))

fatan :: (Eq a, Floating a) => Series a -> Series a
fatan = makeFunction (\n a -> coefs a !!! n)
    where
        coefs a = atan a : [f a x | x <- [0..]]
        base = FracOpt (Poly [Term 1 0]) (Poly [Term 1 2, Term 1 0]) 1
        f a n = fEval (d (fromInteger n) base) a / fromIntegral (fac (n + 1))

(!!!) :: Num a => [a] -> Integer -> a
[] !!! _ = 0
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)


foldExpr :: (Eq a, Floating a) => Expr a -> Series a
foldExpr (Const value) = fromNum value
foldExpr X = x
foldExpr (BinaryOp Add a b) = foldExpr a +: foldExpr b
foldExpr (BinaryOp Subtract a b) = foldExpr a -: foldExpr b
foldExpr (BinaryOp Multiply a b) = foldExpr a *: foldExpr b
foldExpr (BinaryOp Divide a b) = foldExpr a /: foldExpr b
foldExpr (BinaryOp Power _ _) = error "no powers yet"
foldExpr (Function Sin a) = fsin (foldExpr a)
foldExpr (Function Cos a) = fcos (foldExpr a)
foldExpr (Function Atan a) = fatan (foldExpr a)
foldExpr (Function Exp a) = fe (foldExpr a)
foldExpr (Function Ln a) = flog (foldExpr a)
