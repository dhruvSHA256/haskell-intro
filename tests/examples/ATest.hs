{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Prelude hiding (sum, null, length)
import Test.Hspec
import A

deriving instance Eq a => Eq (List a)
deriving instance Eq Nat

main :: IO ()
main = hspec $ do
  describe "A-1" $ do
    it "Example 1" $ isPaper Scissors `shouldBe` False
    it "Example 2" $ isPaper Paper `shouldBe` True
  describe "A-2" $ do
    it "Example 1" $ null [] `shouldBe` True
    it "Example 2" $ null [1, 2, 3] `shouldBe` False
  describe "A-4" $ do
    it "Example 1" $ length [] `shouldBe` 0
    it "Example 2" $ length [1,2,3,4,5] `shouldBe` 5
    it "Example 3" $ length [True, False] `shouldBe` 2
    it "Example 4" $ length "foo" `shouldBe` 3
  describe "A-5" $ do
    it "Example 1" $ noPaper [] `shouldBe` True
    it "Example 2" $ noPaper [Rock, Scissors, Rock] `shouldBe` True
    it "Example 3" $ noPaper [Paper] `shouldBe` False
    it "Example 4" $ noPaper [Rock, Scissors, Paper, Scissors, Rock] `shouldBe` False
  describe "A-6" $ do
    it "Example 1" $ sum [] `shouldBe` 0
    it "Example 2" $ sum [1,2,3] `shouldBe` 6
    it "Example 3" $ sum [1,-1] `shouldBe` 0
    it "Example 4" $ sum [1,99,-3,5] `shouldBe` 102
  describe "A-7" $ do
    it "Example 1" $ from (Cons 1 (Cons 2 Nil)) `shouldBe` [1,2]
    it "Example 2" $ to [1,2] `shouldBe` Cons 1 (Cons 2 Nil)
  describe "A-8" $ do
    it "Example 1" $ evens [1,2,3,4] `shouldBe` [2,4]
    it "Example 1" $ evens [2,8,16,32] `shouldBe` [2,8,16,32]
    it "Example 1" $ evens [] `shouldBe` []
  describe "A-9" $ do
    it "Example 1" $ sumEvenSquares [1,2,3,4] `shouldBe` 20
  describe "A-10" $ do
    it "Example 1" $ allEven [] `shouldBe` True
    it "Example 2" $ allEven [2,4,6] `shouldBe` True
    it "Example 3" $ allEven [1] `shouldBe` False
    it "Example 4" $ allEven [1,2,3] `shouldBe` False
  describe "A-11" $ do
    it "Example 1" $ isAscending [] `shouldBe` True
    it "Example 2" $ isAscending [77] `shouldBe` True
    it "Example 3" $ isAscending [66,55] `shouldBe` False
    it "Example 4" $ isAscending [1,3,3,5] `shouldBe` False
    it "Example 5" $ isAscending [1,3,5,7] `shouldBe` True
    it "Example 6" $ isAscending [1,6,99,200,199,300] `shouldBe` False
  describe "A-12" $ do
    it "Example 1" $ fromNat (Suc (Suc Zero)) `shouldBe` 2
    it "Example 2" $ fromNat Zero `shouldBe` 0
    it "Example 3" $ fromNat (Suc (Suc (Suc (Suc (Suc Zero))))) `shouldBe` 5
  describe "A-13" $ do
    it "Example 1" $ add Zero (Suc Zero) `shouldBe` Suc Zero
    it "Example 2" $ add (Suc Zero) Zero `shouldBe` Suc Zero
    it "Example 2" $ add (Suc Zero) (Suc Zero) `shouldBe` Suc (Suc Zero)

