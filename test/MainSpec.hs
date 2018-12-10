module MainSpec where

import Lib
import Test.Hspec

spec :: Spec
spec = do            
    
    describe "TakeDay" $ do
        it "should return the first 2 charecters of date string" $ do
            Lib.takeDay "23/03/1995" `shouldBe` (23 :: Int)
    
    describe "TakeMonth" $ do
        it "should return the 4th adn 5th charecters of date string" $ do
            Lib.takeMonth "23/03/1995" `shouldBe` (03 :: Int)
    
    describe "TakeYear" $ do
        it "should return the 7th, 8th, 9th, 10th charecters of date string" $ do
            Lib.takeYear "23/03/1995" `shouldBe` (1995 :: Integer)

