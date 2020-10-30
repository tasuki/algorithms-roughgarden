module A1MergeSort
    ( merge, sort
    ) where

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
    let
        (next, tailx, taily) = if x < y
            then (x, xs, y:ys)
            else (y, x:xs, ys)
    in next : (merge tailx taily)

sort :: [Int] -> [Int]
sort [] = []
sort [x] = [x]
sort lst =
    let
        split = div (length lst) 2
        half1 = sort $ take split lst
        half2 = sort $ drop split lst
    in merge half1 half2
