module SimpleSpec where

import Test.Hspec

spec :: Spec
spec = 
    describe "bad_test" $ do
        it "Returns false" $ do
            let x = True
            x `shouldBe` False