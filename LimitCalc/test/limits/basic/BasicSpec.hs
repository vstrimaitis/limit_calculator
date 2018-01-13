module Limits.Basic.BasicSpec (spec) where

import Test.Hspec
import LimitCalc
import LimitCalc.Limits
import LimitCalc.Expr
import Control.Monad

enableProblematicTests = False

spec :: Spec
spec = do
    describe "sin x" $ do
        let expr = Function Sin X
        it "-> 0 as x -> 0" $ do
            let pt = Finite 0
            let lim = findLimit pt expr
            lim `shouldSatisfy` almost 0
        it "-> 1 as x -> pi / 2" $ do
            let pt = Finite (pi / 2)
            let lim = findLimit pt expr
            lim `shouldSatisfy` almost 1
        it "-> -1 as x -> -pi / 2" $ do
            let pt = Finite (-pi / 2)
            let lim = findLimit pt expr
            lim `shouldSatisfy` almost (-1)
        it "has no limit as x -> +inf" $ do
            let pt = PositiveInfinity
            let lim = findLimit pt expr
            lim `shouldSatisfy` nolim
        it "has no limit as x -> -inf" $ do
            let pt = NegativeInfinity
            let lim = findLimit pt expr
            lim `shouldSatisfy` nolim
    describe "cos x" $ do
        let expr = Function Cos X
        it "-> 1 as x -> 0" $ do
            let pt = Finite 0
            let lim = findLimit pt expr
            lim `shouldSatisfy` almost 1
        it "-> 0 as x -> pi / 2" $ do
            let pt = Finite (pi / 2)
            let lim = findLimit pt expr
            lim `shouldSatisfy` almost 0
        it "-> -1 as x -> pi" $ do
            let pt = Finite pi
            let lim = findLimit pt expr
            lim `shouldSatisfy` almost (-1)
        it "has no limit as x -> +inf" $ do
            let pt = PositiveInfinity
            let lim = findLimit pt expr
            lim `shouldSatisfy` nolim
        it "has no limit as x -> -inf" $ do
            let pt = NegativeInfinity
            let lim = findLimit pt expr
            lim `shouldSatisfy` nolim
    describe "exp x" $ do
        let expr = Function Exp X
        it "-> 1 as x -> 0" $ do
            let pt = Finite 0
            let lim = findLimit pt expr
            lim `shouldSatisfy` almost 1
        it "-> e as x -> 1" $ do
            let pt = Finite 1
            let lim = findLimit pt expr
            lim `shouldSatisfy` almost (exp 1)
        it "-> +inf as x -> +inf" $ do
            let pt = PositiveInfinity
            let lim = findLimit pt expr
            lim `shouldSatisfy` pinf
        it "-> 0 as x -> -inf" $ do
            let pt = NegativeInfinity
            let lim = findLimit pt expr
            lim `shouldSatisfy` almost 0
    describe "(1 + x) ^ a" $ do
        let expr = Power (BinaryOp Add (Const 1) X)
        it "-> 6 as x -> 5 with a = 1" $ do
            let pt = Finite 5
            let a = 1
            let lim = findLimit pt (expr a)
            lim `shouldSatisfy` almost 6
        it "-> 1 as x -> -2 with a = 2" $ do
            let pt = Finite (-2)
            let a = 2
            let lim = findLimit pt (expr a)
            lim `shouldSatisfy` almost 1
        it "-> +inf as x -> +inf with a = 2" $ do
            let pt = PositiveInfinity
            let a = 2
            let lim = findLimit pt (expr a)
            lim `shouldSatisfy` pinf
        it "-> 0 as x -> +inf with a = -1" $ do
            let pt = PositiveInfinity
            let a = -1
            let lim = findLimit pt (expr a)
            lim `shouldSatisfy` almost 0
        when enableProblematicTests $
            it "-> +inf as x -> -inf with a = 2" $ do
                let pt = NegativeInfinity
                let a = 2
                let lim = findLimit pt (expr a)
                lim `shouldSatisfy` pinf
        it "has no limit as x -> -1 with a = -1" $ do
            let pt = Finite (-1)
            let a = -1
            let lim = findLimit pt (expr a)
            lim `shouldSatisfy` nolim
    describe "ln (1 + x)" $ do
        let expr = Function Ln (BinaryOp Add (Const 1) X)
        it "-> -inf as x -> -1" $ do
            let pt = Finite (-1)
            let lim = findLimit pt expr
            lim `shouldSatisfy` ninf
        it "-> 0 as x -> 0" $ do
            let pt = Finite 0
            let lim = findLimit pt expr
            lim `shouldSatisfy` almost 0
        it "-> +inf as x -> +inf" $ do
            let pt = PositiveInfinity
            let lim = findLimit pt expr
            lim `shouldSatisfy` pinf
    -- TODO: atan, polynomials


eps = 1e-8

almost expected (HasLimit (Finite actual)) = abs(actual - expected) < eps
almost _ _ = False

pinf (HasLimit PositiveInfinity) = True
pinf _ = False

ninf (HasLimit NegativeInfinity) = True
ninf _ = False

nolim NoLimit = True
nolim _ = False