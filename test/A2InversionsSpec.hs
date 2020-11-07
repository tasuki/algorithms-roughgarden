module A2InversionsSpec where

import Test.Hspec
import A2Inversions

spec :: Spec
spec = do
    describe "Inversions.merge" $ do
        it "merges two lists" $ do
            merge [1, 3, 5] [2, 4, 6] `shouldBe` (3, [1, 2, 3, 4, 5, 6])
        it "merges two lists" $ do
            merge [1, 4, 6] [2, 3, 5] `shouldBe` (5, [1, 2, 3, 4, 5, 6])

    describe "MergeSort.sort" $ do
        it "orders list up to 7" $ do
            inversions [7, 3, 1, 4, 2, 5, 6] `shouldBe` (9, [1, 2, 3, 4, 5, 6, 7])
