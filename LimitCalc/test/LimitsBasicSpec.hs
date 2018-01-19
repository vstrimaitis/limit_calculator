module LimitsBasicSpec (spec) where

import Test.Hspec
import LimitCalc
import LimitCalc.Exact (Exact)
import LimitCalc.Point
import LimitCalc.Sign
import TestsCommon

tests :: (MaybeSigned a, Floating a) => [Test a]
tests =
    [ Test {tInput = "sin x", tX = Finite 0,         tOutput = HasLimit (Finite 0)}
    , Test {tInput = "sin x", tX = Finite (pi/2),    tOutput = HasLimit (Finite 1)}
    , Test {tInput = "sin x", tX = Finite (-pi / 2), tOutput = HasLimit (Finite (-1))}
    , Test {tInput = "sin x", tX = PositiveInfinity, tOutput = NoLimit}
    , Test {tInput = "sin x", tX = NegativeInfinity, tOutput = NoLimit}
    
    , Test {tInput = "cos x", tX = Finite 0,         tOutput = HasLimit (Finite 1)}
    , Test {tInput = "cos x", tX = Finite (pi/2),    tOutput = HasLimit (Finite 0)}
    , Test {tInput = "cos x", tX = Finite pi,        tOutput = HasLimit (Finite (-1))}
    , Test {tInput = "cos x", tX = PositiveInfinity, tOutput = NoLimit}
    , Test {tInput = "cos x", tX = NegativeInfinity, tOutput = NoLimit}
    
    , Test {tInput = "exp x", tX = Finite 0,         tOutput = HasLimit (Finite 1)}
    , Test {tInput = "exp x", tX = Finite 1,         tOutput = HasLimit (Finite (exp 1))}
    , Test {tInput = "exp x", tX = PositiveInfinity, tOutput = HasLimit PositiveInfinity}
    , Test {tInput = "exp x", tX = NegativeInfinity, tOutput = HasLimit (Finite 0)}
    
    --, Test {tInput = "ln (1 + x)", tX = Finite (-1),      tOutput = HasLimit NegativeInfinity} -- ?
    , Test {tInput = "ln (1 + x)", tX = Finite 0,         tOutput = HasLimit (Finite 0)}
    , Test {tInput = "ln (1 + x)", tX = PositiveInfinity, tOutput = HasLimit PositiveInfinity}
    
    , Test {tInput = "atan x", tX = Finite 0,         tOutput = HasLimit (Finite 0)}
    , Test {tInput = "atan x", tX = NegativeInfinity, tOutput = HasLimit (Finite (-pi/2))}
    , Test {tInput = "atan x", tX = PositiveInfinity, tOutput = HasLimit (Finite (pi/2))}
    
    , Test {tInput = "(1 + x) ^ 1",  tX = Finite 5,         tOutput = HasLimit (Finite 6)}
    , Test {tInput = "(1 + x) ^ 2",  tX = Finite (-2),      tOutput = HasLimit (Finite 1)}
    , Test {tInput = "(1 + x) ^ 2",  tX = PositiveInfinity, tOutput = HasLimit PositiveInfinity}
    , Test {tInput = "(1 + x) ^ -1", tX = PositiveInfinity, tOutput = HasLimit (Finite 0)}
    , Test {tInput = "(1 + x) ^ 2",  tX = NegativeInfinity, tOutput = HasLimit PositiveInfinity}
    , Test {tInput = "(1 + x) ^ -1", tX = Finite (-1),      tOutput = NoLimit}
    -- TODO: polynomials
    ]

spec :: Spec
spec = do
    describe "With doubles" $ createTests (tests :: [Test Double])
    describe "With Exact" $ createTests (tests :: [Test Exact])
