module Series
    ( Series
    , Result
    , seriesToCoefs
    , seriesToLim
    , fromNum
    , x
    , seriesToInfo
    , add
    , sub
    , mul
    , divide
    , fsin
    , fcos
    , fe
    , fatan
    , flog
    ) where

import Derivative
import Expr
import qualified Heuristics as H
import Limits
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

data Series a = Series {
    sNeg :: [a],
    sPos :: [a]
}

type Result a = Either (H.Info a) (Series a)

seriesToLim :: (Num a, Ord a) => Series a -> Limit a
seriesToLim s
    | goesToPInf s = HasLimit PositiveInfinity
    | goesToNInf s = HasLimit NegativeInfinity
    | goesToInf  s = NoLimit
    | otherwise    = HasLimit (Finite (safeHead (sPos s)))

seriesToCoefs :: Series a -> ([a], [a])
seriesToCoefs s = (sNeg s, sPos s)

fromNum :: (Num a) => a -> Series a
fromNum d = Series {sNeg = [], sPos = [d]}

seriesToInfo :: (Ord a, Num a) => Series a -> H.Info a
seriesToInfo = H.Info . seriesToLim

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

safeZip :: Num a => (a -> a -> a) -> [a] -> [a] -> [a]
safeZip _ [] [] = []
safeZip f [] (y:ys) = safeZip f [0] (y:ys)
safeZip f (x:xs) [] = safeZip f (x:xs) [0]
safeZip f (x:xs) (y:ys) = f x y : safeZip f xs ys

add :: Num a => Series a -> Series a -> Series a
add s1 s2 = Series { sNeg = safeZip (+) (sNeg s1) (sNeg s2), sPos = safeZip (+) (sPos s1) (sPos s2) }

sub :: Num a => Series a -> Series a -> Series a
sub s1 s2 = Series { sNeg = safeZip (-) (sNeg s1) (sNeg s2), sPos = safeZip (-) (sPos s1) (sPos s2) }

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
mul_ (x:xs) (y:ys) = (x*y) : safeZip (+) d (0:c)
    where
        a = map (*x) ys
        b = map (*y) xs
        c = mul_ xs ys
        d = safeZip (+) a b

mul :: Num a => Series a -> Series a -> Series a
mul s1 s2 = mulBy Series {sNeg = [], sPos = a} (-c1-c2)
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
div1 xs@(x:_) ys@(y:_) = x/y : div1 (safeTail (safeZip (-) xs (map (*(x/y)) ys ))) ys

div2 :: (Eq a, Fractional a) => [a] -> [a] -> Series a
div2 _ [] = error "im broken too"
div2 x (0:ys) = mulBy (div2 x ys) (-1)
div2 x y = Series {sNeg = [], sPos = div1 x y}

divide :: (Eq a, Fractional a) => Series a -> Series a -> Series a
divide s1 s2 = mulBy a (c2-c1)
    where
        a = div2 b1 b2
        b1 = sPos s1'
        b2 = sPos s2'
        s1' = mulBy s1 c1
        s2' = mulBy s2 c2
        c1 = length (sNeg s1)
        c2 = length (sNeg s2)

intPower :: (Eq a, Num a) => Series a -> Integer -> Series a
intPower _ 0 = one
intPower s n = if n `mod` 2 == 0 then result else mul result s
    where
        half = mul s s
        result = intPower half (n `div` 2)

makeFunction :: Floating a => (Integer -> a -> a) -> (Series a -> H.Info a) -> Series a -> Result a
makeFunction deriv heur s 
    | (not . null) (sNeg s) = Left (heur s)
    | otherwise = Right Series { sNeg = [], sPos = map getCoef [0..] }
        where
            getCoef n = (foldl1 (safeZip (+)) (take (fromInteger n + 1) powers) !!! n) * coef n
            a = safeHead (sPos s)
            coefs = 0 : safeTail (sPos s)
            powers = iterate (mul_ coefs) [1]
            coef n = deriv n a

goesToInf :: (Num a, Eq a) => Series a -> Bool
goesToInf s = any ( /= 0) (sNeg s)

goesToPInf :: (Num a, Ord a) => Series a -> Bool
goesToPInf s = fromMaybe False (go (sNeg s))
    where
        go [] = Nothing
        go [x] = go [x, 0]
        go (x:y:xs)
            | x /= 0 = Just False
            | y /= 0 = go xs <|> Just (y > 0)
            | otherwise = go xs

goesToNInf :: (Num a, Ord a) => Series a -> Bool
goesToNInf s = fromMaybe False (go (sNeg s))
    where
        go [] = Nothing
        go [x] = go [x, 0]
        go (x:y:xs)
            | x /= 0 = Just False
            | y /= 0 = go xs <|> Just (y < 0)
            | otherwise = go xs

power :: (Eq a, Num a) => Series a -> a -> Result a
power = makeFunction deriv heur
    where
        deriv n a = deriv' n a (a**n)
        
        deriv' 0 a acc = acc
        deriv' n a acc = deriv' (n-1) a (acc * n / a)

        heur s
            | goesToPInf s = H.Info (HasLimit PositiveInfinity)
            | goesToNInf s = error "Batai"
            | goesToInf  s = error "Batai"
            | otherwise    = H.Info (HasLimit (Finite (safeHead (sPos s) ** a)))

fsin :: (Ord a, Floating a) => Series a -> Result a
fsin = makeFunction (\n a -> deriv n a / fromIntegral (fac n)) heur
    where
        deriv n a
            | n `mod` 4 == 0 = sin a
            | n `mod` 4 == 1 = cos a
            | n `mod` 4 == 2 = -(sin a)
            | otherwise = -(cos a)

        heur s
            | goesToInf s = H.Info NoLimit
            | otherwise = H.Info (HasLimit (Finite (sin (safeHead (sPos s)))))
            
fcos :: (Ord a, Floating a) => Series a -> Result a
fcos = makeFunction (\n a -> deriv n a / fromIntegral (fac n)) heur
    where
        deriv n a
            | n `mod` 4 == 0 = cos a
            | n `mod` 4 == 1 = - (sin a)
            | n `mod` 4 == 2 = -(cos a)
            | otherwise = sin a

        heur s
            | goesToInf s = H.Info NoLimit
            | otherwise = H.Info (HasLimit (Finite (cos (safeHead (sPos s)))))

fe :: (Ord a, Floating a) => Series a -> Result a
fe = makeFunction (\n a -> exp a / fromIntegral (fac n)) heur
    where
        heur s
            | goesToPInf s = H.Info (HasLimit PositiveInfinity)
            | goesToNInf s = H.Info (HasLimit (Finite 0))
            | goesToInf  s = H.Info NoLimit
            | otherwise = H.Info (HasLimit (Finite (exp (safeHead (sPos s)))))

flog :: (Ord a, Floating a) => Series a -> Result a
flog = makeFunction (\n a -> if n == 0 then log a else 1 / (fromInteger n * a ** fromInteger n)) heur
    where
        heur s
            | goesToPInf s = H.Info (HasLimit PositiveInfinity)
            | goesToNInf s = error "batai"
            | goesToInf  s = error "batai"
            | otherwise = H.Info (HasLimit (Finite (log (safeHead (sPos s)))))

fatan :: (Ord a, Floating a) => Series a -> Result a
fatan = makeFunction (\n a -> coefs a !!! n) heur
    where
        coefs a = atan a : [f a x | x <- [0..]]
        base = FracOpt (Poly [Term 1 0]) (Poly [Term 1 2, Term 1 0]) 1
        f a n = fEval (d (fromInteger n) base) a / fromIntegral (fac (n + 1))

        heur s
            | goesToPInf s = H.Info (HasLimit (Finite (pi/2)))
            | goesToNInf s = H.Info (HasLimit (Finite (-pi/2)))
            | goesToInf  s = H.Info NoLimit
            | otherwise    = H.Info (HasLimit (Finite (atan (safeHead (sPos s)))))

(!!!) :: Num a => [a] -> Integer -> a
[] !!! _ = 0
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)