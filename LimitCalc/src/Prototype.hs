module Prototype where

data Series = Series {
    sNeg :: [Double],
    sPos :: [Double]
}

fromDouble :: Double -> Series
fromDouble d = Series {sNeg = [], sPos = [d]}

x :: Series
x = Series {sNeg = [], sPos = [0, 1]}

one :: Series
one = fromDouble 1

sinx :: Series
sinx = Series {sNeg = [], sPos = map f [0..]}
    where 
        f n
            | n `mod` 2 == 0 = 0
            | n `mod` 4 == 1 = 1.0 / fromIntegral (fac n)
            | otherwise = - 1.0 / fromIntegral (fac n)
        
        fac 0 = 1
        fac n = n * fac (n-1)

instance Show Series where
    show s = combine (showNeg (sNeg s)) (showPos (sPos s))
        where
            maxPosTerms = 10

            combine :: String -> String -> String
            combine "" "" = "0"
            combine "" s = s
            combine s "" = s
            combine s t = s ++ " + " ++ t

            showNeg :: [Double] -> String
            showNeg l = joinTerms (showNeg' (-1) l []) " + "

            showPos :: [Double] -> String
            showPos l = joinTerms (reverse (showPos' 0 l [])) " + "
            
            showNeg' :: Int -> [Double] -> [String] -> [String]
            showNeg' _ [] acc = filter (not . null) acc
            showNeg' n (x:xs) acc = showNeg' (n-1) xs (showTerm x n:acc)

            showPos' :: Int -> [Double] -> [String] -> [String]
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
                            | c < 0 = "(" ++ show c ++ ")*"
                            | c == 1 = ""
                            | otherwise = show c ++ "*"
                        e
                            | n < 0 = "^(" ++ show n ++ ")"
                            | n == 1 = ""
                            | otherwise = "^" ++ show n

            joinTerms :: [String] -> String -> String
            joinTerms [] _ = ""
            joinTerms [x] _ = x
            joinTerms (x:xs) sep = x ++ sep ++ joinTerms xs sep

crazyZip :: (Double -> Double -> Double) -> [Double] -> [Double] -> [Double]
crazyZip _ [] [] = []
crazyZip f [] (y:ys) = crazyZip f [0] (y:ys)
crazyZip f (x:xs) [] = crazyZip f (x:xs) [0]
crazyZip f (x:xs) (y:ys) = f x y : crazyZip f xs ys

(+:) :: Series -> Series -> Series
(+:) s1 s2 = Series { sNeg = crazyZip (+) (sNeg s1) (sNeg s2), sPos = crazyZip (+) (sPos s1) (sPos s2) }

(-:) :: Series -> Series -> Series
(-:) s1 s2 = Series { sNeg = crazyZip (-) (sNeg s1) (sNeg s2), sPos = crazyZip (-) (sPos s1) (sPos s2) }

safeHead :: [Double] -> Double
safeHead [] = 0
safeHead (x:xs) = x

safeTail :: [Double] -> [Double]
safeTail [] = []
safeTail (x:xs) = xs

mulBy :: Series -> Int -> Series
mulBy s 0 = s
mulBy s n
    | n < 0     = mulBy (Series {sNeg = safeHead (sPos s) : sNeg s, sPos = safeTail (sPos s)}) (n+1)
    | otherwise = mulBy (Series {sNeg = safeTail (sNeg s), sPos = safeHead (sNeg s) : sPos s}) (n-1) 

mul_ :: [Double] -> [Double] -> [Double]
mul_ [] _ = []
mul_ _ [] = []
mul_ (x:xs) (y:ys) = (x*y) : crazyZip (+) d (0:c)
    where
        a = map (*x) ys
        b = map (*y) xs
        c = mul_ xs ys
        d = crazyZip (+) a b

(*:) :: Series -> Series -> Series
(*:) s1 s2 = mulBy Series {sNeg = [], sPos = a} (-c1-c2)
    where
        a = mul_ b1 b2
        b1 = sPos s1'
        b2 = sPos s2'
        s1' = mulBy s1 c1
        s2' = mulBy s2 c2
        c1 = length (sNeg s1)
        c2 = length (sNeg s2)

div1 :: [Double] -> [Double] -> [Double]
div1 _ [] = error "div by zero"
div1 [] (0:ys) = error "im broken"
div1 [] (_:ys) = []
div1 xs@(x:_) ys@(y:_) = x/y : div1 (safeTail (crazyZip (-) xs (map (*(x/y)) ys ))) ys

div2 :: [Double] -> [Double] -> Series
div2 _ [] = error "im broken too"
div2 x (0:ys) = mulBy (div2 x ys) (-1)
div2 x y = Series {sNeg = [], sPos = div1 x y}

(/:) :: Series -> Series -> Series
(/:) s1 s2 = mulBy a (c2-c1)
    where
        a = div2 b1 b2
        b1 = sPos s1'
        b2 = sPos s2'
        s1' = mulBy s1 c1
        s2' = mulBy s2 c2
        c1 = length (sNeg s1)
        c2 = length (sNeg s2)


