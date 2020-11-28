module A1MergeSort where

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
    if x < y
    then x : (merge xs (y:ys))
    else y : (merge (x:xs) ys)

sort :: [Int] -> [Int]
sort [] = []
sort [x] = [x]
sort lst =
    let
        split = div (length lst) 2
        half1 = sort $ take split lst
        half2 = sort $ drop split lst
    in merge half1 half2
