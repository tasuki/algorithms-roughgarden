module A1Karatsuba where

import Data.Char(digitToInt, intToDigit)
import Data.List(dropWhileEnd)
import Data.Maybe(fromMaybe, listToMaybe)
import Debug.Trace(trace)

multiplyDigits :: [Int] -> [Int] -> [Int]
multiplyDigits xs ys | trace ("multi " ++ show xs ++ ", " ++ show ys) False = undefined
multiplyDigits [] _ = [0]
multiplyDigits _ [] = [0]
multiplyDigits [x] [y] = toDigits $ x * y
multiplyDigits xs ys =
    let
        minlen = min (length xs) (length ys)
        m = max 1 (div minlen 2)

        a = drop m xs -- more significant part of 1st
        b = take m xs -- less significant part of 1st
        c = drop m ys -- more significant part of 2nd
        d = take m ys -- less significant part of 2nd

        z0 = multiplyDigits b d
        z1 = multiplyDigits (add a b) (add c d)
        z2 = multiplyDigits a c

        shifted2 = shift z2 (m * 2)
        shifted1 = shift (sub z1 (add z2 z0)) m
    in add z0 (add shifted1 shifted2)

toDigits :: Int -> [Int]
toDigits = reverse . map digitToInt . show

opDigits :: (Int -> Int -> Int) -> Int -> Int -> Int -> (Int, Int)
opDigits fun x y carried =
    let
        opped = fun (fun x y) carried
        carry = if (opped < 0 || opped > 9) then 1 else 0
    in
    (mod opped 10, carry)

transformDigitsHelper :: Int -> (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
--transformDigitsHelper carried fun xs ys | trace ("transformHelper " ++ show xs ++ " " ++ show ys ++ " " ++ show carried) False = undefined
transformDigitsHelper carried fun [] [] = [carried]
transformDigitsHelper carried fun xs ys =
    let
        listHead :: [Int] -> Int
        listHead = fromMaybe 0 . listToMaybe

        (next, carry) = opDigits fun (listHead xs) (listHead ys) carried
    in next : (transformDigitsHelper carry fun (drop 1 xs) (drop 1 ys))

transformDigits :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
transformDigits fun x y =
    dropWhileEnd (\i -> i == 0) $ transformDigitsHelper 0 fun x y

add :: [Int] -> [Int] -> [Int]
add x y | trace ("add " ++ show x ++ " + " ++ show y) False = undefined
add x y = transformDigits (+) x y

sub :: [Int] -> [Int] -> [Int]
sub x y | trace ("sub " ++ show x ++ " - " ++ show y) False = undefined
sub x y = transformDigits (-) x y

shift :: [Int] -> Int -> [Int]
--shift x y | trace ("shift " ++ show x ++ " * " ++ show y) False = undefined
shift x y | y < 1 = x
shift x y = shift (0 : x) (y - 1)

multiply :: String -> String -> String
multiply x y = concat $ map show $ reverse $ multiplyDigits (readOne x) (readOne y)
    where readOne = reverse . map digitToInt
