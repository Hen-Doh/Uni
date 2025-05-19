{-
ll_parser1 :: String -> Bool
ll_parser1 a = ll_parse a "id_list" 
    where 
    ll_parse :: String -> String -> Bool
    ll_parse (';':'$':['$']) "id_list_tail" = True
    ll_parse (',':'i':'d':xs) "id_list_tail" = ll_parse xs "id_list_tail"
    ll_parse ('i':'d':xs) "id_list" = ll_parse xs "id_list_tail"
    ll_parse _ _ = False 

match :: String -> [String] -> [String]
match t (x:xs) = id_list satz
match a satz = error (show satz) --(concat x ++" is not a valid input for" ++ a)

id_list_tail :: [ String ] -> [ String ]
id_list_tail (";":"$$":[]) =  []
id_list_tail (",":"id":xs) = match "id_list_tail" xs
id_list_tail x = error (concat x ++" is not a valid input for id_list_tail")

id_list :: [ String ] -> [ String ]
id_list ("id":xs) = match "id_list_tail" xs
id_list x = error (show x++ "is not a valid input for id_list")
-}

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

ttail :: Maybe String -> Maybe String
ttail Nothing = Nothing
ttail (Just []) = Just []
ttail (Just [x]) = Just [x]
ttail (Just (x:xs))
    | x== '+' = ttail $ term $ Just xs
    | otherwise = Just (x:xs)


factor :: Maybe String -> Maybe String
factor Nothing = Nothing
factor satz = match 'c' satz

ftail :: Maybe String -> Maybe String
ftail Nothing = Nothing
ftail (Just []) = Just []
ftail (Just [x]) = Just [x]
ftail (Just (x:xs))
    | x== '+' = ftail $ term $ match '+' $  Just ([x]++xs)
    | otherwise =  Just([x] ++ xs)