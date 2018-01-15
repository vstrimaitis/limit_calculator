module LimitsFancySpec where

import Test.Hspec
import LimitCalc
import LimitCalc.Point
import TestsCommon

tests =
    [ Test {tInput = "1 / (1 + x)", tX = Finite (-1),    tOutput = NoLimit}
    , Test {tInput = "exp (x * ln 2) / x ^ 2", tX = Finite 2, tOutput = HasLimit (Finite 1)}
    , Test {tInput = "x ^ 0.2", tX = Finite 30, tOutput = HasLimit (Finite (30**0.2))}
    , Test {tInput = "cos (2*x)", tX = Finite (pi / 2), tOutput = HasLimit (Finite (-1))}
    , Test {tInput = "exp (1/x)", tX = Finite 0, tOutput = NoLimit}
    , Test {tInput = "exp (1/x^2)", tX = Finite 0, tOutput = HasLimit PositiveInfinity}
    , Test {tInput = "sin x / x", tX = Finite 0, tOutput = HasLimit (Finite 1)}
    , Test {tInput = "sin x / x", tX = PositiveInfinity,         tOutput = HasLimit (Finite 0)}
    , Test {tInput = "sin x / cos x", tX = Finite 0, tOutput = HasLimit (Finite 0)}
    , Test {tInput = "cos x / sin x", tX = Finite 0, tOutput = NoLimit}
    , Test {tInput = "1 / x", tX = PositiveInfinity, tOutput = HasLimit (Finite 0)}
    , Test {tInput = "x^2 / x^4", tX = NegativeInfinity, tOutput = HasLimit (Finite 0)}
    , Test {tInput = "x^1.5 / (x*(sqrt(x+1) + sqrt(x-1) + 2*sqrt(x)))", tX = PositiveInfinity, tOutput = HasLimit (Finite 0.25)}
    , Test {tInput = "(exp x * sin x - x * (1 + x))/x^3", tX = Finite 0, tOutput = HasLimit (Finite (1/3))}
    , Test {tInput = "(cos x - exp (-x^2/2))/x^4", tX = Finite 0, tOutput = HasLimit (Finite (-1/12))}
    , Test {tInput = "(5 + x)^5", tX = Finite 5, tOutput = HasLimit (Finite 100000)}
    , Test {tInput = "sqrt 2 * sin x", tX = Finite (pi / 4), tOutput = HasLimit (Finite 1)}
    , Test {tInput = "x^1.5 / (x*(sqrt(x+1) + sqrt(x-1) + 2*sqrt(x)))", tX = Finite 1, tOutput = HasLimit (Finite (1 / (2 + sqrt 2)))}
    , Test {tInput = "x^2 / exp x", tX = PositiveInfinity, tOutput = HasLimit (Finite 0)}
    , Test {tInput = "(sqrt(x+4) - 2)/x", tX = Finite 0, tOutput = HasLimit (Finite (1/4))}
    
    , Test {tInput = "sin (2*atan (1/x))", tX = Finite 0, tOutput = HasLimit (Finite 0)}
    , Test {tInput = "atan (sin x / cos x)", tX = Finite 0, tOutput = HasLimit (Finite 0)}
    , Test {tInput = "atan (exp x - 1 - x) / x^2", tX = Finite 0, tOutput = HasLimit (Finite 0.5)}
    , Test {tInput = "atan (sin x / cos x)", tX = Finite (pi / 2), tOutput = NoLimit}
    , Test {tInput = "x * sin (1/x)", tX = Finite 0, tOutput = HasLimit (Finite 0)}
    , Test {tInput = "sin (4*x) / sin x", tX = Finite 0, tOutput = HasLimit (Finite 4)}
    , Test {tInput = "(((cos x) ^ 2 - 1) * 2 * sin x / cos x)/(1 - (sin x / cos x)^2)", tX = Finite 0, tOutput = HasLimit (Finite 0)}
    
    , Test {tInput = "((sin (x-1))^2 + (cos (x+1))^2 - 1) / (sin x / x - 1)", tX = Finite 0, tOutput = NoLimit}
    , Test {tInput = "(sin x / x - 1) / ((sin (x-1))^2 + (cos (x+1))^2 - 1)", tX = Finite 0, tOutput = HasLimit (Finite 0)}

    , Test {tInput = "x", tX = Finite 30, tOutput = HasLimit (Finite 30)}
    , Test {tInput = "1 + x", tX = Finite (-1), tOutput = HasLimit (Finite 0)}
    , Test {tInput = "1 - x", tX = PositiveInfinity, tOutput = HasLimit NegativeInfinity}
    , Test {tInput = "cos (x)  + sin(x)", tX = Finite (100 * pi) , tOutput = HasLimit (Finite 1)}
    , Test {tInput = "exp (ln x)", tX = Finite 50 , tOutput = HasLimit (Finite 50)}
    , Test {tInput = "sin x * cos x", tX = Finite pi , tOutput = HasLimit (Finite 0)}
    , Test {tInput = "ln (x + 1) / x", tX = Finite 0 , tOutput = HasLimit (Finite 1)}
    , Test {tInput = "( (1 + x)^42 - 1) / x", tX = Finite 0 , tOutput = HasLimit (Finite 42)}
    , Test {tInput = "(e^x - 1) / x", tX = Finite 0 , tOutput = HasLimit (Finite 1)}
    , Test {tInput = "ln x / x^7", tX = PositiveInfinity , tOutput = HasLimit (Finite 0)}
    , Test {tInput = "e^x / x^e", tX = PositiveInfinity , tOutput = HasLimit PositiveInfinity}
    , Test {tInput = "(1 + x)^(1 / x)", tX = PositiveInfinity , tOutput = HasLimit (Finite 1)}
    , Test {tInput = "(1 + x)^(1 / x)", tX = Finite 0 , tOutput = HasLimit (Finite (exp 1))}
    , Test {tInput = "(1 + 1 / x)^(x)", tX = PositiveInfinity , tOutput = HasLimit (Finite (exp 1))}
    , Test {tInput = "x *  (1 / x)", tX = Finite 0 , tOutput = HasLimit (Finite 1)}
    , Test {tInput = "x *  (1 / x)", tX = PositiveInfinity, tOutput = HasLimit (Finite 1)}
    ]

spec :: Spec
spec = createTests tests