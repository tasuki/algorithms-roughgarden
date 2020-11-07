module A1KaratsubaSpec where

import Test.Hspec
import A1Karatsuba

spec :: Spec
spec = do
    describe "helpers" $ do
        it "toDigits" $ do
            toDigits 317 `shouldBe` [7, 1, 3]
        it "opDigits" $ do
            opDigits (+) 5 2 1 `shouldBe` (8, 0)
        it "opDigits" $ do
            opDigits (+) 7 8 1 `shouldBe` (6, 1)
        it "opDigits" $ do
            opDigits (-) 5 2 1 `shouldBe` (2, 0)
        it "opDigits" $ do
            opDigits (-) 7 8 0 `shouldBe` (9, 1)
        it "opDigits" $ do
            opDigits (-) 7 8 3 `shouldBe` (6, 1)

        it "add 7 8" $ do
            add [7] [8] `shouldBe` [5, 1]
        it "add 9 1" $ do
            add [9] [1] `shouldBe` [0, 1]
        it "add 317 82" $ do
            add [7, 1, 3] [8, 2] `shouldBe` [5, 4, 3]
        it "sub 317 82" $ do
            sub [7, 1, 3] [8, 2] `shouldBe` [9, 8, 2]
        it "sub 1024 382" $ do
            sub [4, 2, 0, 1] [2, 8, 3] `shouldBe` [2, 4, 6]
        it "shift 17 by 2" $ do
            shift [7, 1] 2 `shouldBe` [0, 0, 7, 1]

    describe "Karatsuba.multiply" $ do
        it "multiply 7 8" $ do
            multiply "7" "8" `shouldBe` "56"

        it "multiply 7 18" $ do
            multiply "7" "18" `shouldBe` "126"
        it "multiply 18 7" $ do
            multiply "18" "7" `shouldBe` "126"
        it "multiply 19 9" $ do
            multiply "19" "9" `shouldBe` "171"

        it "multiply 14 23" $ do
            multiply "14" "23" `shouldBe` "322"
        it "multiply 123 45" $ do
            multiply "123" "45" `shouldBe` "5535"
        it "multiply 1234 567" $ do
            multiply "1234" "567" `shouldBe` "699678"
        it "multiply 12345 6789" $ do
            multiply "12345" "6789" `shouldBe` "83810205"
