module LimitCalc.Folding (limitAtZero) where

import LimitCalc.Expr
import qualified LimitCalc.Heuristics as H
import qualified LimitCalc.Series as S
import LimitCalc.Series (Result)
import LimitCalc.Limits
import LimitCalc.Point
import LimitCalc.Sign
import LimitCalc.Calc
import Control.Monad ((>=>))

toLimit :: (MaybeSigned a, Num a) => Result a -> Calc (Limit a)
toLimit (Left x) = pure $ H.infoToLim x
toLimit (Right x) = S.seriesToLim x

toInfo :: (MaybeSigned a, Num a) => Result a -> Calc (H.Info a)
toInfo (Left x) = pure $ x
toInfo (Right x) = S.seriesToInfo x

add :: (MaybeSigned a, Num a) => Result a -> Result a -> Calc (Result a)
add (Right a) (Right b) = pure $ Right (S.add a b)
add a b = Left <$> (H.add <$> toInfo a <*> toInfo b)

sub :: (MaybeSigned a, Num a) => Result a -> Result a -> Calc (Result a)
sub (Right a) (Right b) = pure $ Right (S.sub a b)
sub a b = Left <$> (H.sub <$> toInfo a <*> toInfo b)

mul :: (MaybeSigned a, Num a) => Result a -> Result a -> Calc (Result a)
mul (Right a) (Right b) = pure $ Right (S.mul a b)
mul a b = Left <$> (H.mul <$> toInfo a <*> toInfo b)

divide :: (MaybeSigned a, Fractional a) => Result a -> Result a -> Calc (Result a)
divide (Right a) (Right b) = Right <$> S.divide a b
divide a b = Left <$> do
    aa <- toInfo a
    bb <- toInfo b
    H.divide aa bb

intPower :: (MaybeSigned a, Num a) => Result a -> Integer -> Calc (Result a)
intPower (Right a) n = pure $ Right (S.intPower a n)
intPower (Left a) n = pure $ Left $ H.intPower n a

fsin :: (MaybeSigned a, Floating a) => Result a -> Calc (Result a)
fsin (Left a) = pure $ Left $ H.fsin a
fsin (Right a) = S.fsin a

fcos :: (MaybeSigned a, Floating a) => Result a -> Calc (Result a)
fcos (Left a) = pure $ Left $ H.fcos a
fcos (Right a) = S.fcos a

fe :: (MaybeSigned a, Floating a) => Result a -> Calc (Result a)
fe (Left a) = pure $ Left $ H.fe a
fe (Right a) = S.fe a

flog :: (MaybeSigned a, Floating a) => Result a -> Calc (Result a)
flog (Left a) = Left <$> H.flog a
flog (Right a) = S.flog a

fatan :: (MaybeSigned a, Floating a) => Result a -> Calc (Result a)
fatan (Left a) = pure $ Left $ H.fatan a
fatan (Right a) = S.fatan a

lyft :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
lyft f a b = do
    a' <- a
    b' <- b
    f a' b'

foldExpr :: (MaybeSigned a, Floating a) => Expr a -> Calc (Result a)
foldExpr (Const value) = pure $ Right $ S.fromNum value
foldExpr X = pure $ Right S.justX
foldExpr Pi = pure $ Right $ S.fromNum pi
foldExpr (BinaryOp Add a b) = lyft add (foldExpr a) (foldExpr b)
foldExpr (BinaryOp Subtract a b) = lyft sub (foldExpr a) (foldExpr b)
foldExpr (BinaryOp Multiply a b) = lyft mul (foldExpr a) (foldExpr b)
foldExpr (BinaryOp Divide a b) = lyft divide (foldExpr a) (foldExpr b)
foldExpr (IntegerPower a n) = flip intPower n =<< foldExpr a
foldExpr (Function Sin a) = fsin =<< foldExpr a
foldExpr (Function Cos a) = fcos =<< foldExpr a
foldExpr (Function Atan a) = fatan =<< foldExpr a
foldExpr (Function Exp a) = fe =<< foldExpr a
foldExpr (Function Ln a) = flog =<< foldExpr a


limitAtZero :: (MaybeSigned a, Floating a) => Expr a -> Calc (Limit a)
limitAtZero = foldExpr >=> toLimit
