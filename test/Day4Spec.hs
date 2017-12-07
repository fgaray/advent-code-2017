module Day4Spec where

import Test.Hspec
import Day4.A
import Day4.B


spec :: Spec
spec = do
  describe "Day 4 A" $ do
    it "Given input should be 386" $ do
      result <- readFile "day4.input" >>= return . Day4.A.solve . map words . lines
      result `shouldBe` 386

  describe "Day 4 B" $ do
    it "abcde fghij is valid" $ do
      Day4.B.solve [["abcde", "fghij"]] `shouldBe` 1
    it "abcde xyz ecdab" $ do
      Day4.B.solve [["abcde", "xyz", "ecdab"]] `shouldBe` 0
    it "a ab abc abd abf abj" $ do
      Day4.B.solve [["a", "ab", "abc", "abd", "abf", "abj"]] `shouldBe` 1
    it "oiii ioii iioi iiio" $ do
      Day4.B.solve [["oiii", "ioii", "iioi", "iiio"]] `shouldBe` 0
    it "Given input should be 208" $ do
      result <- readFile "day4.input" >>= return . Day4.B.solve . map words . lines
      result `shouldBe` 208
