module Parser.Spec where

import Test.Hspec

spec :: Spec
spec = 
    describe "parser test 1" $ do
        it "Returns empty string" $ do
            let x = ""
            x `shouldBe` ""