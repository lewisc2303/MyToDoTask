module MainSpec where

import Main 
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "test the creation of task deadline creation" $ do
        it "should return Taskdeadline type" $ do
            Main.createTaskDeadLine "23" "10" "2018" `shouldBe` (TaskDeadLine ("23", "10", "2018"))
