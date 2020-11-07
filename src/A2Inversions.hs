module A2Inversions where

-- returns (invs, merged)
merge :: [Int] -> [Int] -> (Int, [Int])
merge [] ys = (0, ys)
merge xs [] = (0, xs)
merge (x:xs) (y:ys) =
    let
        (invs, next, tailx, taily) = if x < y
            then (0, x, xs, y:ys)
            else (length (x:xs), y, x:xs, ys)
        (mergedInvs, merged) = merge tailx taily
    in (invs + mergedInvs, next : merged)

inversions :: [Int] -> (Int, [Int])
inversions [] = (0, [])
inversions [x] = (0, [x])
inversions lst =
    let
        split = div (length lst) 2
        (invs1, half1) = inversions $ take split lst
        (invs2, half2) = inversions $ drop split lst
        (mergedInvs, merged) = merge half1 half2
    in (invs1 + invs2 + mergedInvs, merged)
