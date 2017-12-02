module UtilsSpec where

import Test.Hspec
import Utils


spec :: Spec
spec = do
    describe "String to digits" $ do
        it "1122" $ do
            readDigitsString "1122" `shouldBe` [1, 1, 2, 2]
        it "91212129" $ do
            readDigitsString "91212129" `shouldBe` [9, 1, 2, 1, 2, 1, 2, 9]
