{-# LANGUAGE ScopedTypeVariables #-}
module ZipListSpec where


import Test.Hspec
import ZipList as Z
import Control.Monad (join)


spec :: Spec
spec = do
  describe "ZipList" $ do
    it "Should insert and retrive the first inserted" $ do
      let z = Z.insert (Z.const 1) 2
      (Z.current z) `shouldBe` 1
    it "Should move and retrive the last inserted" $ do
      let z = Z.moveN 1 (Z.insert (Z.const 1) 2)
      (fmap Z.current z) `shouldBe` (Just 2)
    it "Should move and retrive the last inserted" $ do
      let z = join . fmap (Z.moveN (-1)) $ Z.moveN 1 (Z.insert (Z.const 1) 2)
      (fmap Z.current z) `shouldBe` (Just 1)
