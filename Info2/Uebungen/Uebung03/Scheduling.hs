import Data.List

data Prozess = Prozess { pid :: String, arrival :: Int, computing :: Int }
            deriving (Show)

instance Eq Prozess where
    Prozess { computing = a } == Prozess { computing = b } = a == b

instance Ord Prozess where
    compare x y
      | computing x < computing y = LT
      | computing x > computing y = GT
      | otherwise = EQ


data State = State { new :: [Prozess]
                    , run :: Prozess
                    , ready :: [Prozess]
                    , time :: Int
                    , chart :: String }

instance Show State where
    show s = "--new\n" ++ myShowList (new s) ++ "--run\n" ++show (run s) ++"\n"++"--ready\n"++ myShowList (ready s) ++ "\n" ++"--time:"++ show (time s) ++"\n"++ "--chart:" ++ show (chart s)

myShowList :: Show a => [a] -> [Char]
myShowList [] = ""
myShowList [x] = show x ++"\n"
myShowList (x:xs) = show x ++ "\n" ++ myShowList xs

update_ready :: State -> State
update_ready s = State{
    new =  (filter isntReady (new s))
    ,run = run s
    ,ready = ready s ++ filter isReady (new s)
    ,time = time s
    ,chart = chart s
    }
    where
        isReady :: Prozess -> Bool
        isReady a = arrival a == time s
        isntReady :: Prozess -> Bool
        isntReady a = arrival a > time s

sortReady s = State{new =  new s
    ,run =  run s
    ,time = time s
    ,chart = chart s
    ,ready = sort $ ready s
    }

sjf :: State -> State
sjf s = sortReady $ update_ready s

srtf :: State -> State
srtf s = sortReady $ interrupt $ update_ready s -- doch nicht wirklich schöner aber so ist es jetzt
    where
        interrupt s
            |run s == idle =s  
            |otherwise = State{
            new =  new s
            ,run =  idle
            ,ready = [run s] ++ (ready s)
            ,time = time s
            ,chart = chart s
            }


update_run :: State -> State
update_run s
    | run s == idle  = State{
        new =  new s
        ,run =  next (ready s)
        ,ready = drop 1 (ready s)
        ,time = time s
        ,chart = chart s
        }
    | otherwise = s
    where
        next :: [Prozess] -> Prozess
        next [] = idle
        next (x:_) = x

update_time :: State -> State
update_time s = State{
    new =  new s
    ,run =  increaseTime (run s)
    ,ready = ready s
    ,time = time s +1
    ,chart = chart s ++ pid  (run s) ++", "
    }
    where
        increaseTime p
            | computing p == 1 = idle
            | otherwise = Prozess {pid=pid p, arrival=arrival p, computing=(computing p) -1}


idle = Prozess {pid="IDLE", arrival=(-1), computing=(-1)}

exerciseExample = State { 
    new = [  Prozess { pid = " P1 " , arrival =0  , computing =6}
            ,Prozess { pid = " P2 " , arrival =2  , computing =6}
            ,Prozess { pid = " P3 " , arrival =4  , computing =5}
            ,Prozess { pid = " P4 " , arrival =12 , computing =4}
            ,Prozess { pid = " P5 " , arrival =16 , computing =3}
            ,Prozess { pid = " P6 " , arrival =19 , computing =6}]
    , run = idle
    , ready =[]
    , time =0
    , chart =" "}

exerciseExample2 = State { --in exerciseExample kommt es nie zu einem interrupt daher exerciseExample2
    new = [  Prozess { pid = " P1 " , arrival =0  , computing =6}
            ,Prozess { pid = " P2 " , arrival =2  , computing =6}
            ,Prozess { pid = " P3 " , arrival =4  , computing =5}
            ,Prozess { pid = " P4 " , arrival =12 , computing =2}
            ,Prozess { pid = " P5 " , arrival =16 , computing =3}
            ,Prozess { pid = " P6 " , arrival =19 , computing =6}]
    , run = idle
    , ready =[]
    , time =0
    , chart =" "}

run_example :: State -> State
run_example s 
    | new s == [] && run s== idle && ready s== [] = s
    | otherwise = run_example $ update_time $ update_run $ update_ready s

run_sjf :: State -> State
run_sjf s 
    | new s == [] && run s== idle && ready s== [] = s
    | otherwise = run_sjf $ update_time $ update_run $ sjf s

run_srtf :: State -> State
run_srtf s 
    | new s == [] && run s== idle && ready s== [] = s
    | otherwise = run_srtf $ update_time $ update_run $ srtf s  


--  run_example exerciseExample
--  run_sjf exerciseExample
--  run_srtf exerciseExample
--  run_srtf exerciseExample2









ps = [ Prozess { pid = " P1 " , arrival =0 , computing =5},Prozess { pid = " P1 " , arrival =1 , computing =4}]
lectureExample = State { new = ps, run = idle, ready =[], time =0, chart =" "}
