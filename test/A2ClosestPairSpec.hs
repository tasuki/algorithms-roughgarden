module A2ClosestPairSpec where

import Test.Hspec
import A2ClosestPair

spec :: Spec
spec = do
    describe "distance" $ do
        it "should be correct for minus" $ do
            distance (Point 0 0) (Point (-3) (4)) `shouldBe` 5
        it "should be correct when not starting in zero" $ do
            distance (Point 1 1) (Point 4 5) `shouldBe` 5

    describe "closestPoint" $ do
        it "should find closest point" $ do
            closestPoint (Point 0 0)
                [ (Point 2 2)
                , (Point 3 0)
                , (Point 0 3)
                ] `shouldBe` (Pair (Point 0 0) (Point 2 2) (2 * sqrt 2))
        it "should find trickier closest point" $ do
            closestPoint (Point 0 0)
                [ (Point 3 3)
                , (Point 4 0)
                , (Point 0 5)
                ] `shouldBe` (Pair (Point 0 0) (Point 4 0) 4)

    describe "brute" $ do
        it "should find the closes pair something" $ do
            findClosestBrute
                [ Point   0    0
                , Point   3    3
                , Point   5    2
                , Point (-2) (-1)
                , Point (-2)   2
                , Point   6    1
                ] `shouldBe` (Pair (Point 5 2) (Point 6 1) (sqrt 2))
