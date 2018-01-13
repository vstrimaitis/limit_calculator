module Limits.Basic.BasicSpec where

import Test.Hspec

spec :: Spec
spec = 
    describe "test" $ do
        it "Returns true" $ do
            let x = True
            x `shouldBe` True