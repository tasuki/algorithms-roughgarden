module A2ClosestPairSpec where

import Test.Hspec
import A2ClosestPair


fewPoints =
    [ Point   0    0
    , Point   3    3
    , Point   5    2
    , Point (-2) (-1)
    , Point (-2)   2
    , Point   6    1
    ]

manyPoints =
    [ Point (-42) (-28), Point (-9) (-67), Point (-46) (40), Point (-10) (-45), Point (4) (32)
    , Point (11) (-97), Point (-77) (-10), Point (0) (-56), Point (-68) (-22), Point (-19) (-14)
    , Point (-3) (76), Point (37) (-55), Point (54) (60), Point (35) (93), Point (47) (71)
    , Point (-36) (-99), Point (-87) (-5), Point (17) (8), Point (-100) (61), Point (-50) (37)
    , Point (67) (45), Point (73) (-71), Point (78) (7), Point (-36) (73), Point (31) (88)
    , Point (10) (-87), Point (-42) (-100), Point (-78) (47), Point (-67) (35), Point (9) (-62)
    , Point (-40) (37), Point (9) (43), Point (26) (52), Point (-43) (-48), Point (-50) (-40)
    , Point (42) (13), Point (-11) (23), Point (50) (-14), Point (94) (-26), Point (17) (63)
    , Point (61) (51), Point (49) (-91), Point (-18) (48), Point (33) (76), Point (86) (56)
    , Point (57) (1), Point (44) (42), Point (3) (72), Point (-53) (-23), Point (-77) (5)
    , Point (-41) (79), Point (27) (-18), Point (-13) (-96), Point (90) (-59), Point (-49) (-72)
    , Point (18) (-99), Point (15) (-13), Point (30) (18), Point (96) (-57), Point (5) (-91)
    , Point (-46) (-32), Point (55) (-31), Point (41) (76), Point (-89) (-38), Point (70) (-18)
    , Point (86) (94), Point (-55) (-98), Point (-75) (-72), Point (14) (-11), Point (76) (87)
    , Point (-68) (-100), Point (62) (17), Point (-36) (-45), Point (11) (75), Point (18) (19)
    , Point (-17) (30), Point (83) (7), Point (63) (71), Point (73) (-56), Point (20) (99)
    , Point (-57) (-21), Point (11) (85), Point (91) (-14), Point (12) (-2), Point (57) (52)
    , Point (-12) (76), Point (-22) (-36), Point (68) (8), Point (15) (-61), Point (93) (-29)
    , Point (100) (80), Point (32) (41), Point (19) (-54), Point (-26) (53), Point (-53) (-32)
    , Point (44) (-32), Point (-53) (62), Point (-20) (62), Point (-93) (34), Point (95) (35)
    ]


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
        it "should find the closest pair among few points" $ do
            findClosestBrute fewPoints `shouldBe` (Pair (Point 5 2) (Point 6 1) (sqrt 2))

        it "should find the closest pair among many points" $ do
            findClosestBrute manyPoints `shouldBe` (Pair (Point 15 (-13)) (Point 14 (-11)) 2.236068)

    describe "divide conquer" $ do
        it "should find the closest pair among few points" $ do
            findClosestDivideConquer fewPoints `shouldBe` Just (Pair (Point 5 2) (Point 6 1) (sqrt 2))

        it "should find the closest pair among many points" $ do
            findClosestDivideConquer manyPoints `shouldBe` Just (Pair (Point 15 (-13)) (Point 14 (-11)) 2.236068)
