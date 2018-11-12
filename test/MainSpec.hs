module MainSpec where

import Lib (main, test)
import Test.Hspec

main :: IO ()
main = hpsec $ do
    describe "main" $ do
        it "should return 'test'" $ do
            main `shouldBe` ("test")





