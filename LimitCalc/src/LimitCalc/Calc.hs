{-# LANGUAGE DeriveFunctor #-}

module LimitCalc.Calc
    ( Calc(Ok, Undefined, MissingInfo)
    ) where

data Calc a = Ok a | Undefined | MissingInfo deriving (Show, Functor)

instance Applicative Calc where
    pure = Ok
    (Ok f) <*> (Ok x) = Ok $ f x
    Undefined <*> _ = Undefined
    _ <*> Undefined = Undefined
    MissingInfo <*> _ = MissingInfo
    _ <*> MissingInfo = MissingInfo

instance Monad Calc where
    return = Ok
    (Ok x) >>= f = f x
    Undefined >>= _ = Undefined
    MissingInfo >>= _ = MissingInfo
