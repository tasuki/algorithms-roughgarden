module A2ClosestPair where

import Debug.Trace(trace)
import Data.List

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Eq, Show)

data Pair = Pair Point Point Float deriving (Eq, Show)

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
findClosestBrute [] = error "empty list"
findClosestBrute [_] = error "only one element"
findClosestBrute [p1, p2] = pair p1 p2
findClosestBrute (p : ps) = min (closestPoint p ps) (findClosestBrute ps)



findClosestDivideConquer :: [Point] -> Maybe Pair
findClosestDivideConquer [] = Nothing
findClosestDivideConquer [_] = Nothing
findClosestDivideConquer [p1, p2] = Just $ pair p1 p2
findClosestDivideConquer ps =
    let
        byX = sortOn x ps
        byY = sortOn y ps
    in Just $ pair (Point 0 0) (Point 1 1)
