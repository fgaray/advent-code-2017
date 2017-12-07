module Day5Spec where

import Test.Hspec
import Day5.A
import Day5.B


spec :: Spec
spec = do
  describe "Day 5 A" $ do
    it "Example 1" $ do
      Day5.A.solve [0, 3, 0, 1, -3] `shouldBe` 5
  describe "Day 5 B" $ do
    it "Example 1" $ do
      Day5.B.solve [0, 3, 0, 1, -3] `shouldBe` 10
