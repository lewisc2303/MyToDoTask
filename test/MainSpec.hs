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
    
    describe "createList" $ do
        it "should create a list of 10, filled with Nothing type" $ do
            Lib.createList `shouldBe` ([Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing])
    
    