module Main where

import Lib
import Control.Monad (forever)

additemToStore ::[ToDoItem] -> TodoItem -> [TodoItem]
itemStore = toDoList ++ toDoItem 

addItem :: IO()
addItem = forever $ do 
        putStrLn "What is the name of the item?"
        item <- getLine

main :: IO()
main = undefined