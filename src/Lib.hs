{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import System.Exit (exitSuccess)
import Control.Monad (forever)   
import Data.Maybe 
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.ByteString.Lazy as B

data ToDoItem =
        ToDoItem { title :: [Char]
                ,dateAdded :: (Int, Int, Integer)
                ,taskDeadLine :: (Int, Int, Integer)
                ,description :: [Char]
                } deriving (Show, Generic, FromJSON, ToJSON)

currentDay :: IO (Integer,Int,Int) -- :: (year,month,day)
currentDay = getCurrentTime >>= (return . toGregorian . utctDay)

getCurrentDay :: IO (Int,Int,Integer)
getCurrentDay = do
        (x,y,z) <- currentDay
        return (z,y,x)

createToDo :: (Int, Int, Integer) -> String -> String -> IO ToDoItem
createToDo deadline item description = do
        x <- getCurrentDay
        return $ ToDoItem item (x) deadline description

takeDay :: String -> Int
takeDay filteredDate = read (take 2 filteredDate) :: Int

takeMonth :: String -> Int
takeMonth filteredDate = read (take 2 (drop 3 filteredDate)) :: Int 

takeYear :: String -> Integer
takeYear filteredDate = read (take 4 (drop 6 filteredDate)) :: Integer

addItem :: IO()
addItem = do
        putStrLn "What is the name of the item?"
        item <- getLine
        filteredDate <- addDate
        putStrLn "Write a description for this task:"
        description <- getLine
        todo <- createToDo ((takeDay filteredDate), (takeMonth filteredDate), (takeYear filteredDate)) item description
        jsonList <- getJSON
        B.writeFile "data/items2.json" (encode (todo : fromJust jsonList))
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

viewList :: IO()
viewList = do 
        list <- getJSON
        print list

jsonFile :: FilePath
jsonFile = "data/items.json"

getJSON :: IO (Maybe [ToDoItem])
getJSON = do
        list <- decode <$> B.readFile jsonFile
        return (list :: Maybe [ToDoItem])
        