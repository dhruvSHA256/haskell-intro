{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Data.Char
import Test.Hspec
import Test.Hspec.Core.Runner
import D

shouldBeE (AnInt a) (AnInt b) = a `shouldBe` b
shouldBeE (AString a) (AString b) = a `shouldBe` b
shouldBeE _ _ = expectationFailure "values are not equal"

main :: IO ()
main = hspec $ do
  describe "D-1" $ do
    it "Example 1" $ classify " 34   " `shouldBeE` AnInt 34
    it "Example 2" $ classify " - 1" `shouldBeE` AnInt (-1)
    it "Example 3" $ classify "hello" `shouldBeE` AString "hello"
    it "Example 4" $ classify "1!!!1" `shouldBeE` AString "1!!!1"
    it "Example 5" $ classify "42" `shouldBeE` AnInt 42
  describe "D-2" $ do
    it "Example 1" $ AnInt 3 == AnInt 5 `shouldBe` False
    it "Example 2" $ AnInt 4 == AnInt 4 `shouldBe` True
    it "Example 3" $ AnInt 7 == AString "foo" `shouldBe` False
    it "Example 4" $ AString "bar" == AnInt (-12) `shouldBe` False
    it "Example 5" $ AString "foo" == AString "bar" `shouldBe` True
    it "Example 6" $ AString "hello" == AString "" `shouldBe` True
  describe "D-5" $ do
    it "Example 1" $ group [1,1,0,2,2,2] `shouldBe` [1 :| [1],0 :| [],2 :| [2,2]]
    it "Example 2" $ group "hello" `shouldBe` ['h' :| "",'e' :| "",'l' :| "l",'o' :| ""]
    it "Example 3" $ group "aaaaxxaaaax" `shouldBe` ['a' :| "aaa",'x' :| "x",'a' :| "aaa",'x' :| ""]
  describe "D-9" $ do
    it "Example 1" $ sortDescending [1,7,3,4,6,4,5] `shouldBe` [7,6,5,4,4,3,1]
    it "Example 2" $ sortDescending "hello world" `shouldBe` "wroolllhed "
    it "Example 3" $ sortSnds [(1,7), (3,4), (7,6), (5,2), (2,6), (9,1)] `shouldBe` [(9,1),(5,2),(3,4),(7,6),(2,6),(1,7)]
    it "Example 4" $ sortSnds [(True, 2.5), (False, 4.5), (True, 2.7), (False, 9.234)] `shouldBe` [(True,2.5),(True,2.7),(False,4.5),(False,9.234)]
