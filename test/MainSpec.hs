module MainSpec where

import Lib
import Test.Hspec

spec :: Spec
spec = do
    describe "test the creation of TaskDeadline using the createTaskDeadline function" $ do
        it "should return TaskDeadline type" $ do
            Lib.createTaskDeadLine "23" "10" "2018" `shouldBe` (TaskDeadLine ("23", "10", "2018"))
            
    describe "test the creation of ToDoItem from createToDoItem" $ do
        it "should return ToDoItem type" $ do
            Lib.createToDo (TaskDeadLine ("23", "10", "2018")) "item" "description" `shouldBe` 
                (ToDoItem "item" (DateAdded ("13", "11", "2018")) (TaskDeadLine ("23", "10", "2018")) "description")
    
    describe "TakeDay" $ do
        it "should return the first 2 charecters of date string" $ do
            Lib.takeDay "23/03/1995" `shouldBe` ("23")
    
    describe "TakeMonth" $ do
        it "should return the 4th adn 5th charecters of date string" $ do
            Lib.takeMonth "23/03/1995" `shouldBe` ("03")
    
    describe "TakeYear" $ do
        it "should return the 7th, 8th, 9th, 10th charecters of date string" $ do
            Lib.takeYear "23/03/1995" `shouldBe` ("1995")