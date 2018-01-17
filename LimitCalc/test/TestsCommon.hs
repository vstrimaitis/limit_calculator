module TestsCommon
    ( Test (..)
    , createTests)
    where

import Test.Hspec
import LimitCalc
import LimitCalc.Point
import LimitCalc.Expr
-- import LimitCalc.Parsing
import LimitCalc.Sign
import Control.Monad

data Test a = Test {
    tInput  :: String,
    tX      :: Point a,
    tOutput :: Result a
}

createTests :: (Show a, MaybeSigned a, Floating a) => [Test a] -> Spec
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

checkAnswer :: (MaybeSigned a, Floating a, Show a) => Result a -> Result a -> Expectation
checkAnswer lim (HasLimit (Finite expected)) = lim `shouldSatisfy` almost expected
checkAnswer lim (HasLimit PositiveInfinity) = lim `shouldSatisfy` pinf
checkAnswer lim (HasLimit NegativeInfinity) = lim `shouldSatisfy` ninf
checkAnswer lim NoLimit = lim `shouldSatisfy` nolim
checkAnswer lim Undefined = lim `shouldSatisfy` notDefined

createTestDescription t = tInput t ++ showLim (tOutput t) ++ " as x -> " ++ show (tX t)
    where
        showLim NoLimit       = " has no limit"
        showLim Unknown       = error "batai"
        showLim (HasLimit pt) = " -> " ++ show pt

almost :: (MaybeSigned a, Floating a) => a -> Result a -> Bool
almost expected (HasLimit (Finite actual)) = isZero (actual - expected) == Just True
almost _ _ = False

pinf (HasLimit PositiveInfinity) = True
pinf _ = False

ninf (HasLimit NegativeInfinity) = True
ninf _ = False

nolim NoLimit = True
nolim _ = False

notDefined Undefined = True
notDefined _ = False
