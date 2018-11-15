module Lib where

import System.Exit (exitSuccess)
import Control.Monad (forever)   
import Data.Maybe (isJust)

data DateAdded = 
        DateAdded ([Char], [Char], [Char])
                deriving (Eq, Show)
    
data TaskDeadLine =
        TaskDeadLine ([Char], [Char], [Char])
                deriving (Eq, Show)
    
data ToDoItem =
        ToDoItem [Char] DateAdded TaskDeadLine [Char]
                deriving (Eq)
    
instance Show ToDoItem where 
        show (ToDoItem name (DateAdded (d,m,y)) (TaskDeadLine (d1, m1, y1)) description) = 
                "\n" ++
                "Todo Item: " ++ name 
                ++ "\n" ++ "------------------------------------------------"
                ++ "\n" ++ "Created on the " ++ (d ++ "/" ++ m ++ "/" ++ y) ++ " Due by the " ++ (d1 ++ "/" ++ m1 ++ "/" ++ y1)
                ++ "\n" ++ "Description:"
                ++ "\n" ++ description

                
--test item
-- toDoList1 :: ToDoList ToDoItem        
-- toDoList1 = Node (Node None (ToDoItem "name" (DateAdded ("d","m","y")) (TaskDeadLine ("d1", "m1", "y1")) "description") None) (ToDoItem "name" (DateAdded ("d","m","y")) (TaskDeadLine ("d1", "m1", "y1")) "description") None

-- createList :: ToDoItem -> [ToDoItem]
-- createList toDoItem = [] ++ [toDoItem]

-- addToList :: ToDoItem -> [Maybe ToDoItem] -> IO [Maybe ToDoItem]
-- addToList toDoItem list = (Just toDoItem) : list

createToDo :: TaskDeadLine -> String -> String -> ToDoItem
createToDo deadline item description = ToDoItem item (DateAdded ("13", "11", "2018")) deadline description

createTaskDeadLine :: [Char] -> [Char] -> [Char] -> TaskDeadLine
createTaskDeadLine day month year = TaskDeadLine (day, month, year)

takeDay :: String -> String
takeDay date = (take 2 date)

takeMonth :: String -> String
takeMonth date =  (take 2 (drop 3 date))

takeYear :: String -> String
takeYear date = (take 4 (drop 6 date))

addItem :: IO()
addItem = do
        putStrLn "What is the name of the item?"
        item <- getLine
        putStrLn "When is the deadline for this task?"
        date <- getLine
        putStrLn "Write a description for this task:"
        description <- getLine
        let todo = createToDo (createTaskDeadLine (takeDay date) (takeMonth date) (takeYear date)) item description
        writeFile "data/items.txt" (show todo)
        exitSuccess

deleteItem = undefined

viewList :: IO()
viewList = do
        lists <- readFile "data/items.txt"
        exitSuccess

