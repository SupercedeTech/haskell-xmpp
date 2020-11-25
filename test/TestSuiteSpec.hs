
-- | see if we can run any test at all
module TestSuiteSpec where

import Test.Hspec

spec :: Spec
spec = describe "the test suite" $ it "works" $ 1 `shouldBe` 1
