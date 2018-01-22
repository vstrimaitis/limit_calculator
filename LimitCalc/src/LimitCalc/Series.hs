{-# LANGUAGE ScopedTypeVariables #-}

module LimitCalc.Series
    ( Series
    , Result
    , seriesToCoefs
    , seriesToLim
    , fromNum
    , justX
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
    , power
    , intPower
    ) where

import LimitCalc.Derivative
import qualified LimitCalc.Heuristics as H
import LimitCalc.Limits
import LimitCalc.Point
import LimitCalc.Sign
import LimitCalc.Calc

data Series a = Series {
    sNeg :: [a],
    sPos :: [a]
}

type Result a = Either (H.Info a) (Series a)

data Divergence = NegativeInf | PositiveInf | Both

seriesToLim :: (MaybeSigned a, Num a) => Series a -> Calc (Limit a)
seriesToLim s = (\d -> case d of
    Just PositiveInf -> HasLimit PositiveInfinity
    Just NegativeInf -> HasLimit NegativeInfinity
    Just Both -> NoLimit
    Nothing -> HasLimit (Finite (safeHead (sPos s)))) <$> divergence s

seriesToCoefs :: Series a -> ([a], [a])
seriesToCoefs s = (sNeg s, sPos s)

fromNum :: Num a => a -> Series a
fromNum d = Series {sNeg = [], sPos = [d]}

seriesToInfo :: (MaybeSigned a, Num a) => Series a -> Calc (H.Info a)
seriesToInfo = (fmap H.Info) . seriesToLim

justX :: Num a => Series a
justX = Series {sNeg = [], sPos = [0, 1]}

one :: Num a => Series a
one = fromNum 1

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

instance (Show a, Eq a, Num a) => Show (Series a) where
    show series = combine (showNeg (sNeg series)) (showPos (sPos series))
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
safeHead (x:_) = x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

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
div1 _ [] = error "bug: div by zero"
div1 [] (_:_) = []
div1 xs@(x:_) ys@(y:_) = x/y : div1 (safeTail (safeZip (-) xs (map (*(x/y)) ys ))) ys

div2 :: (MaybeSigned a, Fractional a) => [a] -> [a] -> Calc (Series a)
div2 _ [] = breakUndefined
div2 x y@(yh:ys) = case isZero yh of
    Just True -> consumeFuel >> fmap (flip mulBy (-1)) (div2 x ys)
    Just False -> pure Series {sNeg = [], sPos = div1 x y}
    Nothing -> breakUnknown

divide :: forall a. (MaybeSigned a, Fractional a) => Series a -> Series a -> Calc (Series a)
divide s1 s2 = fmap (flip mulBy (c2 - c1)) a
    where
        a :: Calc (Series a)
        a = div2 b1 b2
        b1 = sPos s1'
        b2 = sPos s2'
        s1' = mulBy s1 c1
        s2' = mulBy s2 c2
        c1 = length (sNeg s1)
        c2 = length (sNeg s2)

intPower :: Num a => Series a -> Integer -> Series a
intPower _ 0 = one
intPower s n = if n `mod` 2 == 0 then result else mul result s
    where
        half = mul s s
        result = intPower half (n `div` 2)

makeFunction :: (MaybeSigned a, Floating a)
    => (Integer -> a -> a)
    -> (Divergence -> Calc (H.Info a))
    -> Series a
    -> Calc (Result a)
makeFunction deriv heur s = divergence s >>= \d -> case d of
    Just di -> Left <$> heur di
    Nothing -> pure $ Right Series { sNeg = [], sPos = map getCoef [0..] }
        where
            getCoef n = foldl1 (safeZip (+)) (take (fromInteger n + 1) powers) !!! n
            a = safeHead (sPos s)
            coefs = 0 : safeTail (sPos s)
            powers = zipWith (\n -> map (* coef n)) [0..] (iterate (mul_ coefs) [1])
            coef n = deriv n a

divergence :: MaybeSigned a => Series a -> Calc (Maybe Divergence)
divergence s = go (sNeg s) $ cycle [Both, PositiveInf]
    where
        go :: MaybeSigned a => [a] -> [Divergence] -> Calc (Maybe Divergence)
        go _ [] = error "list should have been infinite"
        go [] _ = pure Nothing
        go (x:xs) (y:ys) = do
            next <- go xs ys
            case next of
                Just di -> pure $ Just di
                Nothing -> case getSign x of
                    Just Positive -> pure $ Just y
                    Just Negative -> pure $ Just $ flipSign y
                    Just Zero -> pure $ Nothing
                    Nothing -> breakUnknown

        flipSign PositiveInf = NegativeInf
        flipSign NegativeInf = PositiveInf
        flipSign Both = Both


data ConvergenceDir = FromPositive | FromNegative | FromBoth deriving Show

convergenceDirection :: MaybeSigned a => Series a -> Calc ConvergenceDir
convergenceDirection series = go (safeTail $ sPos series) $ cycle [FromBoth, FromPositive]
    where
        go :: MaybeSigned a => [a] -> [ConvergenceDir] -> Calc ConvergenceDir
        go _ [] = error "list should have been infinite"
        go [] _ = breakUndefined
        go (x:xs) (y:ys) = consumeFuel >> (case getSign x of
            Just Positive -> pure y
            Just Negative -> pure $ flipSign y
            Just Zero -> go xs ys
            Nothing -> breakUnknown)

        flipSign FromPositive = FromNegative
        flipSign FromNegative = FromPositive
        flipSign FromBoth = FromBoth

power :: (MaybeSigned a, Floating a) => a -> Series a -> Calc (Result a)
power nn = makeFunction deriv heur
    where
        deriv n a = deriv' n a (a**nn)
        
        deriv' 0 _ acc = acc
        deriv' n a acc = deriv' (n-1) a (acc * (nn + 1 - fromInteger n) / (a * fromInteger n))

        heur d = case d of
            PositiveInf -> pure $ H.Info $ HasLimit PositiveInfinity
            NegativeInf -> breakUndefined
            Both -> breakUndefined

fsin :: (MaybeSigned a, Floating a) => Series a -> Calc (Result a)
fsin = makeFunction (\n a -> deriv n a / fromIntegral (fac n)) heur
    where
        deriv n a
            | n `mod` 4 == 0 = sin a
            | n `mod` 4 == 1 = cos a
            | n `mod` 4 == 2 = -(sin a)
            | otherwise = -(cos a)

        heur _ = pure $ H.Info NoLimit
            
fcos :: (MaybeSigned a, Floating a) => Series a -> Calc (Result a)
fcos = makeFunction (\n a -> deriv n a / fromIntegral (fac n)) heur
    where
        deriv n a
            | n `mod` 4 == 0 = cos a
            | n `mod` 4 == 1 = - (sin a)
            | n `mod` 4 == 2 = -(cos a)
            | otherwise = sin a

        heur _ = pure $ H.Info NoLimit

fe :: (MaybeSigned a, Floating a) => Series a -> Calc (Result a)
fe = makeFunction (\n a -> exp a / fromIntegral (fac n)) heur
    where
        heur d = pure $ case d of
            PositiveInf -> H.Info (HasLimit PositiveInfinity)
            NegativeInf -> H.Info (HasLimit (Finite 0))
            Both -> H.Info NoLimit

flog :: (MaybeSigned a, Floating a) => Series a -> Calc (Result a)
flog series = divergence series >>= \d -> case d of
    Just di -> Left <$> heur di
    Nothing -> case getSign (safeHead (sPos series)) of
        Just Positive -> makeFunction der heur series
        Just Negative -> breakUndefined
        Just Zero -> do
            convDir <- convergenceDirection series
            case convDir of
                FromPositive -> pure $ Left $ H.Info (HasLimit NegativeInfinity)
                FromNegative -> breakUndefined
                FromBoth -> pure $ Left $ H.Info (HasLimit NegativeInfinity)
        Nothing -> breakUnknown
    where
        der n a =
            if n == 0 then
                log a
            else
                1 / (fromInteger n * a ** fromInteger n)

        heur d = case d of
            PositiveInf -> pure $ H.Info (HasLimit PositiveInfinity)
            NegativeInf -> breakUndefined
            Both -> pure $ H.Info (HasLimit PositiveInfinity)

fatan :: (MaybeSigned a, Floating a) => Series a -> Calc (Result a)
fatan = makeFunction (\n a -> coefs a !!! n) heur
    where
        coefs a = atan a : [f a x | x <- [0..]]
        base = FracOpt (Poly [Term 1 0]) (Poly [Term 1 2, Term 1 0]) 1
        f a n = fEval (deri (fromInteger n) base) a / fromIntegral (fac (n + 1))

        heur d = pure $ case d of
            PositiveInf -> H.Info (HasLimit (Finite (pi/2)))
            NegativeInf -> H.Info (HasLimit (Finite (-pi/2)))
            Both -> H.Info NoLimit

(!!!) :: Num a => [a] -> Integer -> a
[] !!! _ = 0
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)
