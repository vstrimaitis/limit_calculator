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
    ) where

import Limits

newtype Info a = Info {
    iLim :: Limit a
}

infoToLim :: Info a -> Limit a
infoToLim = iLim

add :: Num a => Info a -> Info a -> Info a
add s1 s2 = undefined

sub :: Num a => Info a -> Info a -> Info a
sub s1 s2 = undefined

mul :: Num a => Info a -> Info a -> Info a
mul s1 s2 = undefined

divide :: (Eq a, Fractional a) => Info a -> Info a -> Info a
divide s1 s2 = undefined

fsin :: Floating a => Info a -> Info a
fsin = undefined

fcos :: Floating a => Info a -> Info a
fcos = undefined

fe :: Floating a => Info a -> Info a
fe = undefined

flog :: Floating a => Info a -> Info a
flog = undefined

fatan :: (Eq a, Floating a) => Info a -> Info a
fatan = undefined