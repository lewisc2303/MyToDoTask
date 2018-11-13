module Lib where

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
                ++ "\n" ++ "------------------------------------------------"
                ++ "\n" ++ "Created on the " ++ (d ++ "/" ++ m ++ "/" ++ y) ++ " Due by the " ++ (d1 ++ "/" ++ m1 ++ "/" ++ y1)
                ++ "\n" ++ "Description:"
                ++ "\n" ++ description