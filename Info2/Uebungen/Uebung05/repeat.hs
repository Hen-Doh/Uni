splitByKey :: Int -> [Int] -> ([Int],[Int])
splitByKey _ [] = ([],[])
splitByKey i (x:xs)
    | x <= i =  (x:fst res,snd res)
    | otherwise = (fst res,x: snd res)
    where 
        res = splitByKey i xs -- nicht sicher ob res 1mal oder 2mal berechnet wird

sortQuick :: [Int] -> [Int]
sortQuick [] = []
sortQuick [x] = [x]
sortQuick (x:xs) = (sortQuick $ fst res) ++ [x] ++ (sortQuick $ snd res)
    where res = splitByKey x xs

--sortQuick [2,1,8,43,8,3,5,0,2,7]

splitInHalf :: [Int]-> ([Int],[Int])
splitInHalf xs =  (take half xs,drop half xs)
    where
        len  =length xs
        half = round $ (fromIntegral len)/2

mergeList :: [Int] -> [Int] -> [Int]
mergeList [] xs = xs
mergeList xs [] = xs
mergeList (x:xs) (y:ys)
    |x < y = [x] ++ mergeList xs (y:ys)
    |otherwise = [y] ++ mergeList (x:xs) ys

sortMerge :: [Int] -> [Int]
sortMerge [] = []
sortMerge [x] = [x]
sortMerge xs = mergeList (sortMerge $ fst splt) (sortMerge $ snd splt)
    where splt = splitInHalf xs

--sortMerge [1,7,3,7,4,2,7,56,4,3,3657,3,36,4,5,7456,2,457]

type Nibble = (Bool,Bool,Bool,Bool)

showNibbleBinnary :: Nibble -> String
showNibbleBinnary (a,b,c,d) = showBin a ++ showBin b ++ showBin c ++ showBin d
    where 
        showBin True = "1"
        showBin False = "0"

showNibbleDual :: Nibble -> String
showNibbleDual (a,b,c,d) = show $ boolMult a 8 + boolMult b 4 + boolMult c 2 + boolMult d 1
    
boolMult :: Bool -> Int -> Int
boolMult True x = x
boolMult False _ = 0 

showNibbleComplement :: Nibble -> String
showNibbleComplement (a,b,c,d) = show $ boolMult b 4 + boolMult c 2 + boolMult d 1 - boolMult a 8

showNibble :: Nibble -> String --Niblle is a type so no instance  Show
showNibble n = showNibbleBinnary n ++ " " ++ showNibbleDual n ++ " " ++ showNibbleComplement n

bitAdder :: Bool-> Bool -> Bool -> (Bool,Bool)
bitAdder a b c = ((a||b)&&(a||c)&&(b||c)  , xor  (xor a b) c)
    where
        xor d e = not ((d&&e)||(not d&& not e))

nibbleAdder :: Nibble -> Nibble -> (Bool,Nibble)
nibbleAdder (a,b,c,d)(e,f,g,h) = 