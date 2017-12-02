module Day1Spec where

import Test.Hspec
import Day1.A
import Day1.B


spec :: Spec
spec = do
    describe "Day 1 A" $ do
        it "1122 is 3" $ do
            Day1.A.solve [1, 1, 2, 2] `shouldBe` 3
        it "1111 is 4" $ do
            Day1.A.solve [1, 1, 1, 1] `shouldBe` 4
        it "1234 is 0" $ do
            Day1.A.solve [1, 2, 3, 4] `shouldBe` 0
        it "91212129 is 0" $ do
            Day1.A.solve [9, 1, 2, 1, 2, 1, 2, 9] `shouldBe` 9

    describe "Day 1 B" $ do
        it "1212 is 6" $ do
            Day1.B.solve [1, 2, 1, 2] `shouldBe` 6
        it "1221 is 0" $ do
            Day1.B.solve [1, 2, 2, 1] `shouldBe` 0
        it "123425 is 4" $ do
            Day1.B.solve [1, 2, 3, 4, 2, 5] `shouldBe` 4
        it "123123 is 12" $ do
            Day1.B.solve [1, 2, 3, 1, 2, 3] `shouldBe` 12
        it "12131415 is 4" $ do
            Day1.B.solve [1, 2, 1, 3, 1, 4, 1, 5] `shouldBe` 4
