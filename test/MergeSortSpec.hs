module MergeSortSpec (spec) where

import Test.Hspec
import MergeSort

spec :: Spec
spec = do
    describe "MergeSort.merge" $ do
        it "merges two lists" $ do
            merge [1, 4, 6] [2, 3, 5] `shouldBe` [1, 2, 3, 4, 5, 6]

    describe "MergeSort.sort" $ do
        it "orders list up to 7" $ do
            sort [7, 3, 1, 4, 2, 5, 6] `shouldBe` [1, 2, 3, 4, 5, 6, 7]
