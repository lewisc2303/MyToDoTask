module Lib where

import System.Exit (exitSuccess)
import Control.Monad (forever)   

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
                
data ToDoList a =
        None 
        | Node (ToDoList a) a (ToDoList a)

--if i use show a in function below then the instance of show ToDoItem isnot used???
instance Show a => Show (ToDoList a) where
    show None = " "
    show (Node (left) a (right)) =
        left ++
        "\n" ++ a ++
        "\n" ++ right

--test item
toDoList1 :: ToDoList ToDoItem        
toDoList1 = Node (Node None (ToDoItem "name" (DateAdded ("d","m","y")) (TaskDeadLine ("d1", "m1", "y1")) "description") None) (ToDoItem "name" (DateAdded ("d","m","y")) (TaskDeadLine ("d1", "m1", "y1")) "description") None

--add ordering at a later date, left or right dependant on the orderig of deadline
-- todo doesent work because of ScopedTypeVariables
createList :: ToDoList ToDoItem -> ToDoItem -> ToDoList ToDoItem
createList None (todo ::ToDoItem) = Node None todo None
createList (Node left a right) (todo :: ToDoItem) = Node a left (createList right todo)

createToDo :: TaskDeadLine -> String  -> String -> ToDoItem
createToDo deadline item description = ToDoItem item (DateAdded ("13", "11", "2018")) deadline description

createTaskDeadLine :: String -> String -> String -> TaskDeadLine
createTaskDeadLine day month year = TaskDeadLine (day, month, year)

addItem :: IO()
addItem = do
        putStrLn "What is the name of the item?"
        item <- getLine
        putStrLn "When is the deadline for this task..."
        putStrLn "day?"
        day <- getLine
        putStrLn "month?"
        month <- getLine
        putStrLn "year?"
        year <- getLine
        putStrLn "Write a description for this task:"
        description <- getLine
        let todo1 = createToDo (createTaskDeadLine day month year) item description
        exitSuccess

deleteItem = undefined

viewList = undefined