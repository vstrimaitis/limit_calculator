module LimitCalc.Folding (limitAtZero, limitAtZero') where

import LimitCalc.Expr
import qualified LimitCalc.Heuristics as H
import qualified LimitCalc.Series as S
import LimitCalc.Series (Result)
import LimitCalc.Limits
import LimitCalc.Point
import LimitCalc.Sign
import LimitCalc.Calc
import Control.Monad ((>=>), liftM2)

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

isGoodLnArg :: (MaybeSigned a, Floating a) => Result a -> Calc Bool
isGoodLnArg result = do
    lim <- either (pure . H.infoToLim) S.seriesToLim result
    pure $ case lim of
        NoLimit -> False
        Unknown -> False
        HasLimit PositiveInfinity -> True
        HasLimit NegativeInfinity -> False
        HasLimit (Finite x) -> case getSign x of
            Just Positive -> True
            _ -> False

foldExpr :: (MaybeSigned a, Floating a) => Expr a -> Calc (Result a)
foldExpr (BinaryOp Add (Function Ln a) (Function Ln b)) = do
    a' <- foldExpr a
    b' <- foldExpr b
    aGood <- isGoodLnArg a'
    bGood <- isGoodLnArg b'
    if aGood && bGood then
        mul a' b' >>= flog
    else
        lyft add (flog a') (flog b')
foldExpr (BinaryOp Subtract (Function Ln a) (Function Ln b)) = do
    a' <- foldExpr a
    b' <- foldExpr b
    aGood <- isGoodLnArg a'
    bGood <- isGoodLnArg b'
    if aGood && bGood then
        divide a' b' >>= flog
    else
        lyft sub (flog a') (flog b')        
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

deepSimplify :: (MaybeSigned a, Num a) => Expr a -> Calc (Expr a)
deepSimplify expr = go expr >>= simplify
    where
        go :: (MaybeSigned a, Num a) => Expr a -> Calc (Expr a)
        go e@(Const _) = pure $ e
        go X = pure $ X
        go Pi = pure $ Pi
        go (BinaryOp Add a b) = liftM2 (BinaryOp Add) (deepSimplify a) (deepSimplify b)
        go (BinaryOp Subtract a b) = liftM2 (BinaryOp Subtract) (deepSimplify a) (deepSimplify b)
        go (BinaryOp Multiply a b) = liftM2 (BinaryOp Multiply) (deepSimplify a) (deepSimplify b)
        go (BinaryOp Divide a b) = liftM2 (BinaryOp Divide) (deepSimplify a) (deepSimplify b)
        go (IntegerPower a n) = flip IntegerPower n <$> deepSimplify a
        go (Function Sin a) = Function Sin <$> deepSimplify a
        go (Function Cos a) = Function Cos <$> deepSimplify a
        go (Function Atan a) = Function Atan <$> deepSimplify a
        go (Function Exp a) = Function Exp <$> deepSimplify a
        go (Function Ln a) = Function Ln <$> deepSimplify a

simplify :: (MaybeSigned a, Num a) => Expr a -> Calc (Expr a)
simplify e@(BinaryOp Multiply (Const _) (Const _)) =
    pure e
simplify (BinaryOp Multiply x y@(Const _)) = do
    consumeFuel
    simplify (BinaryOp Multiply y x)
simplify (BinaryOp Divide (BinaryOp Add a b) c) = do
    consumeFuel
    e1 <- deepSimplify (BinaryOp Divide a c)
    e2 <- deepSimplify (BinaryOp Divide b c)
    simplify (BinaryOp Add e1 e2)
simplify (BinaryOp Divide (BinaryOp Subtract a b) c) = do
    consumeFuel
    e1 <- deepSimplify (BinaryOp Divide a c)
    e2 <- deepSimplify (BinaryOp Divide b c)
    simplify (BinaryOp Subtract e1 e2)
simplify (BinaryOp Divide (Function Exp a) (Function Exp b)) = do
    consumeFuel
    e <- deepSimplify (BinaryOp Subtract a b)
    simplify (Function Exp e)
simplify (BinaryOp Multiply (Function Exp a) (Function Exp b)) = do
    consumeFuel
    e <- deepSimplify (BinaryOp Add a b)
    simplify (Function Exp e)
simplify (BinaryOp Divide (Function Exp a) b) = do
    e <- deepSimplify (BinaryOp Divide b (Function Exp a))
    consumeFuel
    simplify (BinaryOp Divide (Const 1) e)
simplify expr@(BinaryOp Add (BinaryOp Multiply (Const x) a) (BinaryOp Multiply (Const y) b)) = do
    consumeFuel
    case getSign (x - y) of
        Just Zero -> simplify (BinaryOp Multiply (Const x) (BinaryOp Add a b))
        Just _ -> simplify
            (BinaryOp Add
                (BinaryOp Multiply (Const x) (BinaryOp Add a b))
                (BinaryOp Multiply (BinaryOp Subtract (Const x) (Const y)) b))
        Nothing -> pure expr
simplify expr@(BinaryOp Subtract (BinaryOp Multiply (Const x) a) (BinaryOp Multiply (Const y) b)) = do
    consumeFuel
    case getSign (x - y) of
        Just Zero -> simplify (BinaryOp Multiply (Const x) (BinaryOp Subtract a b))
        Just _ -> simplify
            (BinaryOp Subtract
                (BinaryOp Multiply (Const x) (BinaryOp Subtract a b))
                (BinaryOp Multiply (BinaryOp Subtract (Const x) (Const y)) b))
        Nothing -> pure expr
simplify e = pure e

simp :: (MaybeSigned a, Num a) => Expr a -> CalcResult (Expr a)
simp = runWithInfinite . simplify

limitAtZero :: (MaybeSigned a, Floating a) => Expr a -> Calc (Limit a)
limitAtZero = deepSimplify >=> foldExpr >=> toLimit

limitAtZero' :: (MaybeSigned a, Floating a) => Expr a -> Calc (S.Series a)
limitAtZero' = deepSimplify >=> foldExpr >=> (pure . either (error "got left") id)
