module Day3Spec where

import Test.Hspec
import Prelude hiding (Right, Left)
import Day3.A
import Day3.B
import Utils


spec :: Spec
spec = do
    describe "move" $ do
        it "move Right 1 step" $ do
            move (0, 0) 1 Right `shouldBe` [(1, 0)]
        it "move Right 2 step" $ do
            move (-1, -1) 2 Right `shouldBe` [(0, -1), (1, -1)]
    
    describe "coords" $ do
        it "Give the first coord" $ do
            (coords (0, 0) 1) !! 0 `shouldBe` (0, 0)
        it "Give the second coord" $ do
            (coords (0, 0) 1) !! 1 `shouldBe` (1, 0)
        it "Give the coord of 15" $ do
            (coords (0, 0) 1) !! (15 - 1) `shouldBe` (0, 2)
        it "Give the coord of 18" $ do
            (coords (0, 0) 1) !! (18 - 1) `shouldBe` (-2, 1)
        it "Give the coord of 19" $ do
            (coords (0, 0) 1) !! (19 - 1) `shouldBe` (-2, 0)

    describe "Day 3 A" $ do
        it "1 is 0" $ do
            Day3.A.solve 1 `shouldBe` 0
        it "10 is 3" $ do
            Day3.A.solve 10 `shouldBe` 3
        it "12 is 3" $ do
            Day3.A.solve 12 `shouldBe` 3
        it "13 is 4" $ do
            Day3.A.solve 13 `shouldBe` 4
        it "15 is 2" $ do
            Day3.A.solve 15 `shouldBe` 2
        it "16 is 3" $ do
            Day3.A.solve 16 `shouldBe` 3
        it "17 is 4" $ do
            Day3.A.solve 17 `shouldBe` 4
        it "18 is 3" $ do
            Day3.A.solve 18 `shouldBe` 3
        it "19 is 2" $ do
            Day3.A.solve 19 `shouldBe` 2
        it "20 is 3" $ do
            Day3.A.solve 20 `shouldBe` 3
        it "21 is 4" $ do
            Day3.A.solve 21 `shouldBe` 4
        it "23 is 2" $ do
            Day3.A.solve 23 `shouldBe` 2
        it "1024 is 31" $ do
            Day3.A.solve 1024 `shouldBe` 31
        it "My input 277678 is 475" $ do
            Day3.A.solve 277678 `shouldBe` 475

    describe "Day 3 B" $ do
        let cs = take 10 (coords (0, 0) 1)
            replaced = map fst . reverse . replaceSum . reverse . addNumbersToCoords $ cs
        it "Replaced 1" $ do
            replaced !! 0 `shouldBe` 1
        it "Replaced 2" $ do
            replaced !! 1 `shouldBe` 1
        it "Replaced 3" $ do
            replaced !! 2 `shouldBe` 2
        it "Replaced 4" $ do
            replaced !! 3 `shouldBe` 4
        it "Replaced 6" $ do
            replaced !! 5 `shouldBe` 10
