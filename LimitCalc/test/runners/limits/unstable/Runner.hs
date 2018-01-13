import Test.Hspec

import qualified Limits.Unstable.SimpleSpec as Unstable

main :: IO ()
main = hspec $ do
    describe "Unstable" Unstable.spec