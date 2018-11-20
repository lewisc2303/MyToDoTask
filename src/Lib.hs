{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Lib where

import System.Exit (exitSuccess)
import Control.Monad (forever)   
import Data.Maybe (isJust)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.Calendar
import qualified System.IO.Strict as SIO

data DateAdded = 
        DateAdded (Int, Int, Integer)
                deriving (Eq, Show, Generic)
    
data TaskDeadLine =
        TaskDeadLine (Int, Int, Integer)
                deriving (Eq, Show, Generic)
    
data ToDoItem =
        ToDoItem [Char] (DateAdded) TaskDeadLine [Char]
                deriving (Show, Generic)

currentDay :: IO (Integer,Int,Int) -- :: (year,month,day)
currentDay = getCurrentTime >>= (return . toGregorian . utctDay)

getCurrentDay :: IO DateAdded
getCurrentDay = do
        (x,y,z) <- currentDay
        return (DateAdded (z,y,x))

createToDo :: TaskDeadLine -> String -> String -> IO ToDoItem
createToDo deadline item description = do
        x <- getCurrentDay
        return $ ToDoItem item (x) deadline description

createTaskDeadLine :: Int -> Int -> Integer -> TaskDeadLine
createTaskDeadLine day month year = TaskDeadLine (day, month, year)

takeDay :: String -> Int
takeDay filteredDate = read (take 2 filteredDate) :: Int

takeMonth :: String -> Int
takeMonth filteredDate = read (take 2 (drop 3 filteredDate)) :: Int 

takeYear :: String -> Integer
takeYear filteredDate = read (take 4 (drop 6 filteredDate)) :: Integer

concatListAndNewItem :: ToDoItem -> IO () 
concatListAndNewItem toDoItem = do
                        list <- SIO.readFile "data/items.txt"
                        writeFile "data/items.txt" (list ++ "\n" ++ (show toDoItem))

addItem :: IO()
addItem = do
        putStrLn "What is the name of the item?"
        item <- getLine
        filteredDate <- addDate
        putStrLn "Write a description for this task:"
        description <- getLine
        todo <- createToDo (createTaskDeadLine (takeDay filteredDate) (takeMonth filteredDate) (takeYear filteredDate)) item description
        concatListAndNewItem todo
        exitSuccess

deleteItem = undefined

addDate :: IO String
addDate = do
        putStrLn "When is the deadline for this task?"
        date <- getLine
        case (length date) of
                10 -> return date
                _  -> do
                        putStrLn "Your date must be in the form dd/mm/yyy"
                        addDate

viewList :: IO ()
viewList = do
        lists <- readFile "data/items.txt"
        print lists
        exitSuccess
