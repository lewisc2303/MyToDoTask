module Main where

import Lib
import System.Exit (exitSuccess)
import Control.Monad (forever)

    
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

main :: IO()
main = do
        addItem