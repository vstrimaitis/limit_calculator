import Test.Hspec

import qualified Limits.Unstable.SimpleSpec as Unstable
import qualified Limits.Basic.BasicSpec as Basic

main :: IO ()
main = hspec $ do
    describe "Basic"    Basic.spec
    describe "Unstable" Unstable.spec