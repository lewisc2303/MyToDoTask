module MainSpec where

import Lib (main, test)
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Test for test environment" $ do
        it "should return 'test'" $ do
            Lib.main `shouldBe` (3)