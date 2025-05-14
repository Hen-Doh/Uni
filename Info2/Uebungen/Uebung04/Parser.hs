LLparser1 :: [char] -> bool
LLparser1 (";$$") = true
LLparser1 (id:xs) = 