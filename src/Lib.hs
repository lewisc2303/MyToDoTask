{-# LANGUAGE FlexibleInstances #-}

module Lib where

import System.Exit (exitSuccess)
import Control.Monad (forever)   
import Data.Maybe (isJust)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.Calendar

data DateAdded = 
        DateAdded (Int, Int, Integer)
                deriving (Eq, Show)
    
data TaskDeadLine =
        TaskDeadLine ([Char], [Char], [Char])
                deriving (Eq, Show)
    
data ToDoItem =
        ToDoItem [Char] (DateAdded) TaskDeadLine [Char]
                deriving (Show)
    
-- instance (Show a, Show b) => Show (IO DateAdded) where
--         show a b = show (unsafePerformIO (read a b))

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

createTaskDeadLine :: [Char] -> [Char] -> [Char] -> TaskDeadLine
createTaskDeadLine day month year = TaskDeadLine (day, month, year)

takeDay :: String -> String
takeDay filteredDate = (take 2 filteredDate)

takeMonth :: String -> String
takeMonth filteredDate =  (take 2 (drop 3 filteredDate))

takeYear :: String -> String
takeYear filteredDate = (take 4 (drop 6 filteredDate))

addItem :: IO()
addItem = do
        putStrLn "What is the name of the item?"
        item <- getLine
        filteredDate <- addDate
        putStrLn "Write a description for this task:"
        description <- getLine
        todo <- createToDo (createTaskDeadLine (takeDay filteredDate) (takeMonth filteredDate) (takeYear filteredDate)) item description
        writeFile "data/items.txt" (show todo)
        exitSuccess

deleteItem = undefined

addDate :: IO String
addDate = do 
        putStrLn "When is the deadline for this task?"
        date <- getLine
        case (length date) of 
                10 -> return date
                _ -> do 
                        putStrLn "Your date must be in the form dd/mm/yyy"
                        addDate

viewList :: IO ()
viewList = do
        lists <- readFile "data/items.txt"
        exitSuccess

