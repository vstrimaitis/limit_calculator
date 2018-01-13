module Folding (limitAtZero) where

import Expr
import qualified Heuristics as H
import qualified Series as S
import Series (Result)
import Limits

toLimit :: (Num a, Ord a) => Result a -> Limit a
toLimit (Left x) = H.infoToLim x
toLimit (Right x) = S.seriesToLim x

toInfo :: Result a -> H.Info a
toInfo (Left x) = x
toInfo (Right x) = S.seriesToInfo x

add :: Num a => Result a -> Result a -> Result a
add (Right a) (Right b) = Right (S.add a b)
add a b = Left $ H.add (toInfo a) (toInfo b)

sub :: Num a => Result a -> Result a -> Result a
sub (Right a) (Right b) = Right (S.sub a b)
sub a b = Left $ H.sub (toInfo a) (toInfo b)

mul :: Num a => Result a -> Result a -> Result a
mul (Right a) (Right b) = Right (S.mul a b)
mul a b = Left $ H.mul (toInfo a) (toInfo b)

divide :: (Eq a, Fractional a) => Result a -> Result a -> Result a
divide (Right a) (Right b) = Right (S.divide a b)
divide a b = Left $ H.divide (toInfo a) (toInfo b)

fsin :: (Ord a, Floating a) => Result a -> Result a
fsin (Left a) = Left $ H.fsin a
fsin (Right a) = S.fsin a
            
fcos :: (Ord a, Floating a) => Result a -> Result a
fcos (Left a) = Left $ H.fcos a
fcos (Right a) = S.fcos a

fe :: (Ord a, Floating a) => Result a -> Result a
fe (Left a) = Left $ H.fe a
fe (Right a) = S.fe a

flog :: (Ord a, Floating a) => Result a -> Result a
flog (Left a) = Left $ H.flog a
flog (Right a) = S.flog a

fatan :: (Ord a, Floating a) => Result a -> Result a
fatan (Left a) = Left $ H.fatan a
fatan (Right a) = S.fatan a



foldExpr :: (Ord a, Floating a) => Expr a -> Result a
foldExpr (Const value) = Right $ S.fromNum value
foldExpr X = Right S.x
foldExpr (BinaryOp Add a b) = foldExpr a `add` foldExpr b
foldExpr (BinaryOp Subtract a b) = foldExpr a `sub` foldExpr b
foldExpr (BinaryOp Multiply a b) = foldExpr a `mul` foldExpr b
foldExpr (BinaryOp Divide a b) = foldExpr a `divide` foldExpr b
foldExpr (BinaryOp Power _ _) = error "no powers yet"
foldExpr (Function Sin a) = fsin (foldExpr a)
foldExpr (Function Cos a) = fcos (foldExpr a)
foldExpr (Function Atan a) = fatan (foldExpr a)
foldExpr (Function Exp a) = fe (foldExpr a)
foldExpr (Function Ln a) = flog (foldExpr a)


limitAtZero :: (Ord a, Floating a) => Expr a -> Limit a
limitAtZero = toLimit . foldExpr