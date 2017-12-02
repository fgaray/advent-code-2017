module Day2Spec where

import Test.Hspec
import Day2.A
import Day2.B


spec :: Spec
spec = do
    describe "Day 2 A" $ do
        it "18" $ do
            let matrix = [[5, 1, 9, 5], [7, 5, 3], [2, 4, 6, 8]]
            Day2.A.solve matrix `shouldBe` 18
        it "9" $ do
            let matrix = [[5, 9, 2, 8], [9, 4, 7, 3], [3, 8, 6, 5]]
            Day2.B.solve matrix `shouldBe` 9
