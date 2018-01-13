module Heuristics 
    ( Info (Info)
    , infoToLim
    , add
    , sub
    , mul
    , divide
    , fsin
    , fcos
    , fe
    , fatan
    , flog
    , power
    , intPower
    ) where

import LimitCalc.Limits

newtype Info a = Info {
    iLim :: Limit a
} deriving Show

infoToLim :: Info a -> Limit a
infoToLim = iLim

lift2 :: (Limit a -> Limit a -> Limit a) -> (Info a -> Info a -> Info a)
lift2 f (Info a) (Info b) = Info $ f a b

lift :: (Limit a -> Limit a) -> (Info a -> Info a)
lift f (Info a) = Info $ f a

add :: Num a => Info a -> Info a -> Info a
add = lift2 f
    where
        f Unknown _ = Unknown
        f _ Unknown = Unknown
        f (HasLimit PositiveInfinity) NoLimit = Unknown
        f NoLimit (HasLimit PositiveInfinity) = Unknown
        f (HasLimit NegativeInfinity) NoLimit = Unknown
        f NoLimit (HasLimit NegativeInfinity) = Unknown
        f (HasLimit (Finite _)) NoLimit = NoLimit
        f NoLimit (HasLimit (Finite _)) = NoLimit
        f (HasLimit (Finite _)) (HasLimit PositiveInfinity) = HasLimit PositiveInfinity
        f (HasLimit (Finite _)) (HasLimit NegativeInfinity) = HasLimit NegativeInfinity
        f (HasLimit PositiveInfinity) (HasLimit (Finite _)) = HasLimit PositiveInfinity
        f (HasLimit NegativeInfinity) (HasLimit (Finite _)) = HasLimit NegativeInfinity
        f (HasLimit (Finite a)) (HasLimit (Finite b)) = HasLimit (Finite (a + b)) 
        f (HasLimit PositiveInfinity) (HasLimit NegativeInfinity) = Unknown
        f (HasLimit NegativeInfinity) (HasLimit PositiveInfinity) = Unknown
        f (HasLimit NegativeInfinity) (HasLimit NegativeInfinity) = HasLimit NegativeInfinity
        f (HasLimit PositiveInfinity) (HasLimit PositiveInfinity) = HasLimit PositiveInfinity
        f NoLimit NoLimit = Unknown
        
sub :: Num a => Info a -> Info a -> Info a
sub a b = add a (lift neg b)
    where
        neg (HasLimit (Finite x)) = HasLimit (Finite (-x))
        neg (HasLimit PositiveInfinity) = HasLimit NegativeInfinity
        neg (HasLimit NegativeInfinity) = HasLimit PositiveInfinity
        neg x = x

mul :: (Ord a, Num a) => Info a -> Info a -> Info a
mul = lift2 f
    where
        f Unknown _ = Unknown
        f _ Unknown = Unknown
        f (HasLimit PositiveInfinity) NoLimit = Unknown
        f NoLimit (HasLimit PositiveInfinity) = Unknown
        f (HasLimit NegativeInfinity) NoLimit = Unknown
        f NoLimit (HasLimit NegativeInfinity) = Unknown
        f (HasLimit (Finite _)) NoLimit = Unknown
        f NoLimit (HasLimit (Finite _)) = Unknown
        f (HasLimit (Finite x)) (HasLimit PositiveInfinity)
            | x > 0 = HasLimit PositiveInfinity
            | x < 0 = HasLimit NegativeInfinity
            | otherwise = Unknown
        f (HasLimit (Finite x)) (HasLimit NegativeInfinity)
            | x > 0 = HasLimit NegativeInfinity
            | x < 0 = HasLimit PositiveInfinity
            | otherwise = Unknown
        f (HasLimit PositiveInfinity) (HasLimit (Finite x))
            | x > 0 = HasLimit PositiveInfinity
            | x < 0 = HasLimit NegativeInfinity
            | otherwise = Unknown
        f (HasLimit NegativeInfinity) (HasLimit (Finite x))
            | x > 0 = HasLimit NegativeInfinity
            | x < 0 = HasLimit PositiveInfinity
            | otherwise = Unknown
        f (HasLimit (Finite a)) (HasLimit (Finite b)) = HasLimit (Finite (a * b)) 
        f (HasLimit PositiveInfinity) (HasLimit NegativeInfinity) = HasLimit NegativeInfinity
        f (HasLimit NegativeInfinity) (HasLimit PositiveInfinity) = HasLimit NegativeInfinity
        f (HasLimit NegativeInfinity) (HasLimit NegativeInfinity) = HasLimit PositiveInfinity
        f (HasLimit PositiveInfinity) (HasLimit PositiveInfinity) = HasLimit PositiveInfinity
        f NoLimit NoLimit = Unknown

divide :: (Ord a, Fractional a) => Info a -> Info a -> Info a
divide a b = mul a (lift inv b)
    where
        inv (HasLimit (Finite 0)) = Unknown
        inv (HasLimit (Finite x)) = HasLimit (Finite (1 / x))
        inv (HasLimit PositiveInfinity) = HasLimit (Finite 0)
        inv (HasLimit NegativeInfinity) = HasLimit (Finite 0)
        inv x = x

intPower :: (Ord a, Num a) => Integer -> Info a -> Info a
intPower n = lift f
    where
        f NoLimit = Unknown -- atan(1/x)^2, x -> 0
        f Unknown = Unknown
        f (HasLimit PositiveInfinity) = HasLimit PositiveInfinity
        f (HasLimit NegativeInfinity) = HasLimit $ if n `mod` 2 == 0 then PositiveInfinity else NegativeInfinity
        f (HasLimit (Finite x)) = HasLimit (Finite (x ^ n))

power :: (Ord a, Floating a) => a -> Info a -> Info a
power n = lift f
    where
        f NoLimit = NoLimit
        f Unknown = Unknown
        f (HasLimit PositiveInfinity) = HasLimit PositiveInfinity
        f (HasLimit NegativeInfinity) = error "Batai"
        f (HasLimit (Finite x))
            | x < 0     = error "Batai"
            | otherwise = HasLimit (Finite (x ** n))

fsin :: Floating a => Info a -> Info a
fsin = lift f
    where
        f NoLimit = Unknown -- sin(2*atan(1/x)), x -> 0
        f Unknown = Unknown
        f (HasLimit PositiveInfinity) = NoLimit
        f (HasLimit NegativeInfinity) = NoLimit
        f (HasLimit (Finite x)) = HasLimit (Finite (sin x))

fcos :: Floating a => Info a -> Info a
fcos = lift f
    where
        f NoLimit = Unknown -- cos(2*atan(1/x)), x -> 0
        f Unknown = Unknown
        f (HasLimit PositiveInfinity) = NoLimit
        f (HasLimit NegativeInfinity) = NoLimit
        f (HasLimit (Finite x)) = HasLimit (Finite (cos x))

fe :: Floating a => Info a -> Info a
fe = lift f
    where
        f NoLimit = NoLimit
        f Unknown = Unknown
        f (HasLimit PositiveInfinity) = HasLimit PositiveInfinity
        f (HasLimit NegativeInfinity) = HasLimit (Finite 0)
        f (HasLimit (Finite x)) = HasLimit (Finite (exp x))

flog :: Floating a => Info a -> Info a
flog = lift f
    where
        f NoLimit = NoLimit
        f Unknown = Unknown
        f (HasLimit PositiveInfinity) = HasLimit PositiveInfinity
        f (HasLimit NegativeInfinity) = error "Batai"
        f (HasLimit (Finite x)) = HasLimit (Finite (log x)) -- x < 0 evil

fatan :: (Eq a, Floating a) => Info a -> Info a
fatan = lift f
    where
        f NoLimit = NoLimit
        f Unknown = Unknown
        f (HasLimit PositiveInfinity) = HasLimit (Finite (pi/2))
        f (HasLimit NegativeInfinity) = HasLimit (Finite (-pi/2))
        f (HasLimit (Finite x)) = HasLimit (Finite (atan x))
