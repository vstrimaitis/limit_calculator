import Test.Hspec

import qualified Parser.Spec as Something

main :: IO ()
main = hspec $ do
    describe "Something"    Something.spec