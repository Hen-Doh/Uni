import Data.Char ( toLower )

foldList :: ( Double -> Double -> Double ) -> [ Double ] -> Double 
foldList (op) [x] = x
foldList (op) (x:xs) =  op x (foldList op xs)

mapList :: (Int->Int) -> [Int] -> [Int]
mapList (op) [x] = [op x]
mapList (op) (x:xs) = [op x] ++ mapList (op) xs

square :: Int -> Int
square a = a*a

containsList :: [Int] -> Int -> Bool
containsList [] _ = False
containsList [x] a = x==a
containsList (x:xs) a 
    | a ==x = True
    | otherwise = containsList xs a      
--containsList [-100..100] (-21)        

countList :: [Char] -> Char -> Int
countList [] _ = 0
countList(x:xs) a
    |(toLower x) == a = 1 + countList xs a 
    |otherwise =countList xs a
--countList ['A','a','b','c'] 'a'