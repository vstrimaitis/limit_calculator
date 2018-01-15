{-# LANGUAGE DeriveFunctor #-}

module LimitCalc.Calc
    ( CalcResult(Ok, Undefined, MissingInfo)
    , Calc
    , runCalc
    , breakUndefined
    , breakUnknown
    , consumeFuel
    , runWithInfinite
    ) where

import Control.Monad (ap)

data CalcResult a = Ok a | Undefined | MissingInfo deriving (Show, Functor)

data Calc a = Calc { runCalc :: Integer -> CalcResult (Either (Calc a) (Integer, a)) }

runWithInfinite :: Calc a -> CalcResult a
runWithInfinite fueled = case runCalc fueled 1 of
    Ok (Left fueled') -> runWithInfinite fueled'
    Ok (Right (_, value)) -> Ok value
    Undefined -> Undefined
    MissingInfo -> MissingInfo

instance Show (Calc a) where
    show _ = "<calc>"

class Monad m => FuelMonad m where
    consumeFuel :: m ()

class Monad m => BreakingMonad m where
    breakUndefined :: m a
    breakUnknown :: m a

instance Applicative CalcResult where
    pure = Ok
    (Ok f) <*> (Ok x) = Ok $ f x
    Undefined <*> _ = Undefined
    _ <*> Undefined = Undefined
    MissingInfo <*> _ = MissingInfo
    _ <*> MissingInfo = MissingInfo

instance Monad CalcResult where
    return = Ok
    (Ok x) >>= f = f x
    Undefined >>= _ = Undefined
    MissingInfo >>= _ = MissingInfo

instance Functor Calc where
    fmap f = Calc . fmap (fmap (mapBoth (fmap f) (mapSnd f))) . runCalc
        where
            mapBoth g _ (Left x) = Left $ g x
            mapBoth _ g (Right x) = Right $ g x
            mapSnd g (x, y) = (x, g y)

instance Applicative Calc where
    pure = return
    (<*>) = ap

instance Monad Calc where
    return x = Calc $ \fuel -> Ok $ Right (fuel, x)
    x >>= f = Calc $ \fuel -> case runCalc x fuel of
        Undefined -> Undefined
        MissingInfo -> MissingInfo
        Ok (Left fueled) -> Ok $ Left $ fueled >>= f
        Ok (Right (fuel', value)) -> runCalc (f value) fuel'

instance BreakingMonad Calc where
    breakUndefined = Calc $ const Undefined
    breakUnknown = Calc $ const MissingInfo

instance FuelMonad Calc where
    consumeFuel = Calc $ \fuel -> Ok $
        if fuel <= 0 then
            Left consumeFuel
        else
            Right (fuel - 1, ())
