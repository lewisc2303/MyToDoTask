module Str where

toString :: Int -> String
toString n = (read (show n) :: String)

split :: String -> [Int]
split str = map (\a -> read a :: Int) str

product' :: Int -> Int
product' = product . split . toString