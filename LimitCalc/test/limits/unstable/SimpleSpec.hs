module Limits.Unstable.SimpleSpec where

import Test.Hspec

spec :: Spec
spec = 
    describe "good_test" $ do
        it "Returns false" $ do
            let x = False
            x `shouldBe` False