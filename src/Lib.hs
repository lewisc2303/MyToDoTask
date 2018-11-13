module Lib where

--import Data.Dates

import System.Exit (exitSuccess)
import Control.Monad (forever)

main :: IO()
main = print test

test :: Int
test = 3

data DateAdded = 
    DateAdded ([Char], [Char], [Char])
    deriving Show

data TaskDeadLine =
    TaskDeadLine ([Char], [Char], [Char])
    deriving Show

data ToDoItem =
    ToDoItem [Char] DateAdded TaskDeadLine [Char]

instance Show ToDoItem where 
    show (ToDoItem name (DateAdded (d,m,y)) (TaskDeadLine (d1, m1, y1)) description) = 
        "\n" ++
        "Todo Item: " ++ name 
        ++ "\n" ++ "-------------------------------------------------------"
        ++ "\n" ++ "Created on the " ++ (d ++ "/" ++ m ++ "/" ++ y) ++ " Due by the " ++ (d1 ++ "/" ++ m1 ++ "/" ++ y1)
        ++ "\n" ++ "Description:"
        ++ "\n" ++ description
            
itemStore :: [Maybe ToDoItem]
itemStore = undefined

additemToStore ::ToDoItem -> [Maybe ToDoItem]
additemToStore toDoItem = undefined 

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
    print todo1