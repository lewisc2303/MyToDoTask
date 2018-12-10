module Main where

import Lib
import System.Exit (exitSuccess)
import Control.Monad (forever)

main :: IO()
main = forever $ do
        putStrLn "Enter 'a' to add an item, 'd' to delete an item, 'v' to view the ToDoList"
        option <- getLine
        case option of 
                "a" -> addItem 
                "d" -> deleteItem
                "v" -> viewList
                _   -> putStrLn "Your must enter either 'a', 'd' or 'v'" 