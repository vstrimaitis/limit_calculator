module TestsCommon
    ( Test (..)
    , createTests)
    where

import Test.Hspec
import LimitCalc
import LimitCalc.Limits
import LimitCalc.Expr
import LimitCalc.Parsing
import Control.Monad

data Test a = Test {
    tInput  :: String,
    tX      :: Point a,
    tOutput :: Limit a
}

createTests [] = return ()
createTests (t:ts) = do
    it (createTestDescription t) $ do
        let pt = tX t
        let expr = parseExpr (tInput t)
        case expr of
            Left err -> expectationFailure "Parse failed"
            Right ex -> do
                let lim = findLimit pt ex
                checkAnswer lim (tOutput t)
    createTests ts

checkAnswer :: (Ord a, Floating a, Show a) => Limit a -> Limit a -> Expectation
checkAnswer lim (HasLimit (Finite expected)) = lim `shouldSatisfy` almost expected
checkAnswer lim (HasLimit PositiveInfinity) = lim `shouldSatisfy` pinf
checkAnswer lim (HasLimit NegativeInfinity) = lim `shouldSatisfy` ninf
checkAnswer lim NoLimit = lim `shouldSatisfy` nolim

createTestDescription t = tInput t ++ showLim (tOutput t) ++ " as x -> " ++ show (tX t)
    where
        showLim NoLimit       = " has no limit"
        showLim Unknown       = error "batai"
        showLim (HasLimit pt) = " -> " ++ show pt

eps :: (Ord a, Floating a) => a
eps = 1e-8

almost :: (Ord a, Floating a) => a -> Limit a -> Bool
almost expected (HasLimit (Finite actual)) = abs(actual - expected) < eps
almost _ _ = False

pinf (HasLimit PositiveInfinity) = True
pinf _ = False

ninf (HasLimit NegativeInfinity) = True
ninf _ = False

nolim NoLimit = True
nolim _ = False