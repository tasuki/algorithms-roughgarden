module A2ClosestPair where

import Debug.Trace(trace)
import Data.List
import Data.Maybe
import qualified Data.Vector as V

maxSize = 1000000 -- lazy

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Eq, Show)

data Pair = Pair Point Point Float deriving (Eq, Show)

pairDist :: Pair -> Float
pairDist (Pair _ _ dst) = dst

instance Ord Pair where
  compare (Pair _ _ d1) (Pair _ _ d2) = compare d1 d2


distance :: Point -> Point -> Float
distance p1 p2 =
    sqrt $ fromIntegral (((x p1) - (x p2)) ^ 2
                       + ((y p1) - (y p2)) ^ 2)

pair :: Point -> Point -> Pair
pair p1 p2 = Pair p1 p2 (distance p1 p2)

closestPoint :: Point -> [Point] -> Pair
closestPoint point points = minimum $ map (pair point) points

findClosestBrute :: [Point] -> Pair
findClosestBrute ps | length ps <= 1 = error "not enough for a pair"
findClosestBrute [p1, p2] = pair p1 p2
findClosestBrute (p : ps) = min (closestPoint p ps) (findClosestBrute ps)


findClosestWithinSix :: [Point] -> Maybe Pair
findClosestWithinSix ps | trace ("closest six: " ++ show ps) False = undefined
findClosestWithinSix ps | length ps <= 1 = Nothing
findClosestWithinSix [p1, p2] = Just $ pair p1 p2
findClosestWithinSix (p : ps) =
    let
        closestFromFirst :: Pair
        closestFromFirst = closestPoint p $ take 6 ps
    in
    case (findClosestWithinSix ps) of
        Just tailMin -> Just $ min closestFromFirst tailMin
        Nothing -> Just closestFromFirst


minMaybe :: Ord a => [a] -> Maybe a
minMaybe [] = Nothing
minMaybe xs = Just $ minimum xs

flatten :: [Maybe a] -> [a]
flatten = Data.Maybe.mapMaybe id

divideConquerVec :: V.Vector Point -> Maybe Pair
divideConquerVec ps | trace ("div conq: " ++ show ps) False = undefined
divideConquerVec ps | length ps <= 1 = Nothing
divideConquerVec ps | length ps == 2 = Just $ pair (ps V.! 0) (ps V.! 1)
divideConquerVec ps =
    let
        -- recursively find minimum distance for left and right half
        half = (V.length ps) `div` 2
        (lHalf, rHalf) = V.splitAt half ps
        minL = divideConquerVec lHalf
        minR = divideConquerVec rHalf

        -- find minimum distance for middle stripe
        stripeCenter = fromIntegral $ x $ ps V.! half
        stripeWidth = fromMaybe maxSize $ fmap pairDist $ minMaybe $ flatten [minL, minR]
        stripeFilter p = (floatX p < stripeCenter + stripeWidth) && (floatX p > stripeCenter - stripeWidth)
            where floatX = fromIntegral . x
        minStripe = findClosestWithinSix $ sortOn y $ filter stripeFilter $ V.toList ps
    in minMaybe $ flatten [minL, minR, minStripe]

findClosestDivideConquer :: [Point] -> Maybe Pair
findClosestDivideConquer ps =
    divideConquerVec $ V.fromList $ sortOn x ps
