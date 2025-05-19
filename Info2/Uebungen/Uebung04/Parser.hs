
match1 :: String -> [String] -> [String]
match1 t [] = error ("[] is not matching the expected "++ t)
match1 t (x:xs) 
    | t == x = xs
    | otherwise = error (x++ "is not matching the expected "++ t ++ " remaining: "++show xs)

id_list_tail :: [String] -> [String]
id_list_tail [] = []
id_list_tail (x:xs)
    |x==";" = id_list_tail $ match1 "$$" $ match1 ";" (x:xs) -- das 2. match sollte auch durch xs ersetzt werden können
    |x == "," = id_list_tail $ match1 "id"xs
    |otherwise = error (x++show xs ++"is not valid in id_list_tail")

id_list :: [ String ] -> [ String ]
id_list satz = id_list_tail $ match1 "id" satz

--id_list [ "id" , "," , "id" , ";" , "$$" ]
--id_list [ "id" , "," , "id" , ";" , "$" ]
test1 :: [String]
test1 = id_list [ "id" , "," , "id" , ";" , "$$" ]

test2 :: [String]
test2 = id_list [ "id" , "," , "id" , ";" , "$" ]

-------------------------------------------2. Parser--------------------------------------------------------


match :: Char -> Maybe String -> Maybe String
match _ Nothing = Nothing
match _ (Just []) = Nothing
match a (Just [x]) 
    | a == x = Just []
    | otherwise = Nothing
match a (Just (x:xs))
    | a == x =  Just xs
    | otherwise = Nothing

prog :: String -> Maybe String
prog satz = match '$' $ expr $ Just satz    -- "$"" wird als letztes überprüft

expr :: Maybe String -> Maybe String
expr Nothing = Nothing
expr satz = ttail $ term satz

term :: Maybe String -> Maybe String
term Nothing = Nothing
term satz = ftail $ factor satz

factor :: Maybe String -> Maybe String
factor Nothing = Nothing
factor satz = match 'c' satz

ttail :: Maybe String -> Maybe String
ttail Nothing = Nothing
ttail (Just (x:xs))
    | x== '+' = ttail $ term $ Just xs
    | otherwise = Just (x:xs)
ttail _ = Nothing -- how to check for empty string/single item string?

ftail :: Maybe String -> Maybe String
ftail Nothing = Nothing
ftail (Just (x:xs))
    | x== '*' = ftail $ term $ match '*' $  Just ([x]++xs)
    | otherwise =  Just([x] ++ xs)
ftail _ = Nothing

test3:: Maybe String
test3 = prog "c+c*c$"

test4:: Maybe String
test4 = prog "c+cc$"

test5:: Maybe String
test5 = prog "c+*c"

test6:: Maybe String
test6 = prog "*c$"