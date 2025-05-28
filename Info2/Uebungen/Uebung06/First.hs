import Data.Map qualified as Map
import Data.List

data Symbol = N String | T String | Epsilon deriving ( Ord )
instance Show Symbol where
    show (N s) = "<"++s++">" 
    show (T s) = s 
    show Epsilon = "Epsilon"

type Production = [ Symbol ]

data Rule = Rule { lhs :: Symbol , rhs :: [ Production ]}
instance Show Rule where
    show r = show (lhs r) ++ " -> "++ (intercalate " | " $ map show  (rhs r)  )




data Grammar = Grammar [Rule]
instance Show Grammar where
    show (Grammar []) = ""
    show (Grammar [x]) = show x
    show (Grammar (x:xs)) = intercalate " /n " $ map show (x:xs) 

rules :: Grammar -> [Rule]
rules (Grammar r) = r

instance Eq Symbol where
    (==) ( N n1 ) ( N n2 ) = n1 == n2
    (==) ( T t1 ) ( T t2 ) = t1 == t2
    (==) Epsilon Epsilon = True
    (==) _ _ = False

removeEpsilon :: [ Symbol ] -> [ Symbol ]
removeEpsilon [] = []
removeEpsilon [Epsilon] = []
removeEpsilon (Epsilon:xs) = removeEpsilon xs
removeEpsilon (x:xs) =[x] ++ removeEpsilon xs 

containsEpsilon :: [ Symbol ] -> Bool
containsEpsilon [] = False
containsEpsilon [x] = x == Epsilon
containsEpsilon (Epsilon:_) = True
containsEpsilon (x:xs) = containsEpsilon xs 

findRules :: Symbol -> [ Rule ] -> [ Production ]
findRules s [] = []
findRules s [r]
        |lhs r == s = rhs r
        |otherwise = []
findRules s (r:xs)
        |lhs r == s = rhs r ++ findRules s xs
        |otherwise = findRules s xs

firstSetProd :: Grammar -> Production -> [Symbol]
firstSetProd g [] = []
firstSetProd g [(N n)] = map firstSetProd g $ findRules (N n) (rules g)
firstSetProd g [(T t)] = [(T t)]
firstSetProd g [Epsilon] = [Epsilon]
firstSetProd g ((T t):xs) = [(T t)]
firstSetProd g (Epsilon:xs) = [Epsilon] --da müsste aber echt was schief lafuen um hier anzukommen
firstSetProd g ((N n):xs)
    |hasEpsilon = firstSetProd g xs
    |otherwise = firstSetProd g $ findRules (N n) (rules g)
    where 
        rulesWithN = findRules (N n) (rules g)
        hasEpsilon = foldl (map containsEpsilon rulesWithN)


e1 = N " E1 " 
e2 = N " E2 "
f = N " F "
t1 = N " T1 "
t2 = N " T2 "
_id = T " id "
pl = T " + "
mul = T " * "
po = T " ( " ; pc = T " ) "
e = Epsilon


r1 = Rule { lhs = e1 , rhs = [[ t1 , e2 ]]}
r2 = Rule { lhs = e2 , rhs = [[ pl , t1 , e2 ] , [ e ]]}
r3 = Rule { lhs = t1 , rhs = [[ f , t2 ]]}
r4 = Rule { lhs =t2 , rhs = [[ mul , f , t2 ] , [ e ]]}
r5 = Rule { lhs =f , rhs = [[ po , e1 , pc ] , [ _id ]]}
g = Grammar [ r1 ,r2 , r3 , r4 , r5 ]

test = findRules e1 (rules g)