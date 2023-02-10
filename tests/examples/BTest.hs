{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Prelude hiding (replicate)

import Test.Hspec
import B

deriving instance Eq Month

main :: IO ()
main = hspec $ do
  describe "B-1" $ do
    it "Example 1" $ collapse (Left [1,2,3]) `shouldBe` 6
    it "Example 2" $ collapse (Right 77) `shouldBe` 77
    it "Example 3" $ collapse (Left []) `shouldBe` 0
  describe "B-2" $ do
    it "Example 1" $ mkMonth 0 `shouldBe` Nothing
    it "Example 2" $ mkMonth 1 `shouldBe` Just (MkMonth 1)
    it "Example 3" $ mkMonth 7 `shouldBe` Just (MkMonth 7)
    it "Example 4" $ mkMonth 12 `shouldBe` Just (MkMonth 12)
    it "Example 5" $ mkMonth 13 `shouldBe` Nothing
    it "Example 6" $ mkMonth (-2) `shouldBe` Nothing
  describe "B-3" $ do
    it "Example 1" $ mapMaybe (+1) (Just 3) `shouldBe` Just 4
    it "Example 2" $ mapMaybe (*2) Nothing `shouldBe` Nothing
    it "Example 3" $ mapMaybe length (Just "hello") `shouldBe` Just 5
  describe "B-4" $ do
    it "Example 1" $ pairMaybe (Just 3) (Just 4) `shouldBe` Just (3,4)
    it "Example 2" $ pairMaybe (Just 'x') Nothing `shouldBe` (Nothing :: Maybe (Char, Int))
    it "Example 3" $ pairMaybe (Just "foo") (Just False) `shouldBe` Just ("foo", False)
    it "Example 4" $ pairMaybe Nothing Nothing `shouldBe` (Nothing :: Maybe ((), ()))
  describe "B-5" $ do
    it "Example 1" $ liftMaybe (+) (Just 3) (Just 7) `shouldBe` Just 10
    it "Example 2" $ liftMaybe (+) (Just 3) Nothing `shouldBe` Nothing
    it "Example 3" $ liftMaybe (\ x _ -> x) Nothing (Just 7) `shouldBe` (Nothing :: Maybe Int)
    it "Example 4" $ liftMaybe (\ x _ -> x) (Just "foo") (Just "bar") `shouldBe` Just "foo"
    it "Example 5" $ liftMaybe (++) (Just "foo") (Just "bar") `shouldBe` Just "foobar"
  describe "B-7" $ do
    let table = [(1, "x"), (2, "y"), (3, "z")]
    it "Example 1" $ lookups [1,2,3] table `shouldBe` Just ["x", "y", "z"]
    it "Example 2" $ lookups [3,1] table `shouldBe` Just ["z", "x"]
    it "Example 3" $ lookups [2,2] table `shouldBe` Just ["y", "y"]
    it "Example 4" $ lookups [] table `shouldBe` Just []
    it "Example 5" $ lookups [7] table `shouldBe` Nothing
    it "Example 6" $ lookups [1,2,3,4] table `shouldBe` Nothing
    it "Example 7" $ lookups [2,0,2] table `shouldBe` Nothing
  describe "B-9" $ do
    it "Example 1" $ reverseAcc [1,2,3] [] `shouldBe` [1,2,3]
    it "Example 2" $ reverseAcc [3,2,1] [4,5,6] `shouldBe` [6,5,4,3,2,1]
  describe "B-11" $ do
    it "Example 1" $ replicate 3 'x' `shouldBe` "xxx"
    it "Example 2" $ replicate 5 False `shouldBe` [False, False, False, False, False]
    it "Example 3" $ replicate 0 5 `shouldBe` []
    it "Example 4" $ replicate (-3) [1,2,3] `shouldBe` []
  describe "B-15" $ do
    it "Example 1" $ sameShape tree1 tree1 `shouldBe` True
    it "Example 2" $ sameShape tree3 tree2 `shouldBe` False
    it "Example 3" $ sameShape tree5 tree2 `shouldBe` True
    it "Example 4" $ sameShape tree3 tree3 `shouldBe` True
  describe "B-17" $ do
    it "Example 1" $ buildTree 0 `shouldBe` Leaf ()
    it "Example 2" $ buildTree 1 `shouldBe` Node (Leaf ()) (Leaf ())
    it "Example 3" $ buildTree 2 `shouldBe` Node (Node (Leaf ()) (Leaf ())) (Node (Leaf ()) (Leaf ()))
    it "Example 4" $ buildTree (-5) `shouldBe` Leaf ()
  describe "B-18" $ do
    it "Example 1" $ prop_eval1 `shouldBe` True
    it "Example 2" $ prop_eval2 `shouldBe` True
  describe "B-19" $ do
    it "Example 1" $ countOps expr1 `shouldBe` 2
    it "Example 2" $ countOps expr2 `shouldBe` 3
