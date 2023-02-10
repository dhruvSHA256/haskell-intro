{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Data.Char
import Test.Hspec
import Test.Hspec.Core.Runner
import F

deriving instance Eq Month

main :: IO ()
main = hspec $ do
  describe "F-1" $ do
    it "Example 1" $ validateMonths [] `shouldBe` Just []
    it "Example 2" $ validateMonths [1,4,7,10] `shouldBe` Just [MkMonth 1,MkMonth 4,MkMonth 7,MkMonth 10]
    it "Example 3" $ validateMonths [1,4,7,13] `shouldBe` Nothing
    it "Example 4" $ validateMonths [1,-2,7,10] `shouldBe` Nothing
  describe "F-2" $ do
    it "Example 1" $ foldM (\ a x -> if odd a then Nothing else Just (a + x)) 0 [1..10] `shouldBe` Nothing
    it "Example 2" $ foldM (\ a x -> if odd a then Nothing else Just (a + x)) 0 [2,2,2,2] `shouldBe` Just 8
  describe "F-3" $ do
    it "Example 1" $ navigate exampleLevel entrance (replicate 10 E) `shouldBe` Just (RoomId 6)
    it "Example 2" $ navigate exampleLevel entrance [E,N,N] `shouldBe` Just (RoomId 7)
    it "Example 3" $ navigate exampleLevel entrance [E,N,S,W,W,W] `shouldBe` Nothing
  describe "F-5" $ do
    it "Example 1" $ runProgram (Return (Var "x")) `shouldBe` 0
    it "Example 2" $ runProgram (Assign "x" (Lit 3) $ Assign "x" (Add (Var "x") (Lit 1)) $ Return (Var "x")) `shouldBe` 4
    it "Example 3" $ runProgram (Assign "x" (Lit 3) $ Assign "y" (Add (Var "x") (Var "x")) $ Assign "x" (Add (Var "x") (Var "y")) $ Return (Var "x")) `shouldBe` 9
