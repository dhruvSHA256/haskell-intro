{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Prelude hiding (concat, inits)

import Test.Hspec
import Test.Hspec.Core.Runner
import C

deriving instance Eq Pos

main :: IO ()
main = hspec $ do
  describe "C-3" $ do
    it "Example 1" $ countWhiteSpaceStrings ["  x ", "  ", ""] `shouldBe` 2
    it "Example 2" $ countWhiteSpaceStrings ["  x "] `shouldBe` 0
    it "Example 3" $ countWhiteSpaceStrings [] `shouldBe` 0
    it "Example 4" $ countWhiteSpaceStrings [""] `shouldBe` 1
    it "Example 5" $ countWhiteSpaceStrings ["a", "b", "c", "d"] `shouldBe` 0
    it "Example 6" $ countWhiteSpaceStrings ["foo", "   ", "bar", " ", "baz", "   ", "      "] `shouldBe` 4
  describe "C-4" $ do
    it "Example 1" $ removeLongWords 8 "Haskell is a wonderful language" `shouldBe` "Haskell is a language"
    it "Example 2" $ removeLongWords 2 "Haskell is a wonderful language" `shouldBe` "is a"
    it "Example 3" $ removeLongWords 1 "Haskell is a wonderful language" `shouldBe` "a"
    it "Example 4" $ removeLongWords 0 "Haskell is a wonderful language" `shouldBe` ""
    it "Example 5" $ removeLongWords 10 "  Superfluous  whitespace    is removed   " `shouldBe` "whitespace is removed"
    it "Example 6" $ removeLongWords 14 "  Superfluous  whitespace    is removed   " `shouldBe` "Superfluous whitespace is removed"
  describe "C-5" $ do
    it "Example 1" $ concat [[1,2,3], [4,5], [], [6,7]] `shouldBe` [1,2,3,4,5,6,7]
    it "Example 2" $ concat @() [] `shouldBe` []
    it "Example 3" $ concat @() [[]] `shouldBe` []
    it "Example 4" $ concat ["hello", "world"] `shouldBe` "helloworld"
  describe "C-6" $ do
    it "Example 1" $ goDir origin Up `shouldBe` MkPos {pUp = 1, pRight = 0}
    it "Example 2" $ goDir (MkPos 5 5) Lft `shouldBe` MkPos {pUp = 5, pRight = 4}
    it "Example 3" $ goDir (MkPos (-3) 0) Down `shouldBe` MkPos {pUp = -4, pRight = 0}
    it "Example 4" $ goDirs origin [Up,Rgt,Down,Lft] `shouldBe` MkPos {pUp = 0, pRight = 0}
    it "Example 5" $ goDirs origin [Rgt,Rgt,Rgt,Down,Down] `shouldBe` MkPos {pUp = -2, pRight = 3}
    it "Example 6" $ goDirs (MkPos (-2) 3) [Lft,Lft,Lft,Up,Up] `shouldBe` MkPos {pUp = 0, pRight = 0}
    it "Example 7" $ goDirs (MkPos 1 2) [] `shouldBe` MkPos {pUp = 1, pRight = 2}
    it "Example 8" $ goDirs origin (replicate 10000000 Up) `shouldBe` MkPos {pUp = 10000000, pRight = 0}
  describe "C-7" $ do
    it "Example 1" $ inits [1,2,3] `shouldBe` [[],[1],[1,2],[1,2,3]]
    it "Example 2" $ inits "Haskell" `shouldBe` ["","H","Ha","Has","Hask","Haske","Haskel","Haskell"]
    it "Example 3" $ inits [] `shouldBe` [[]]
  describe "C-8" $ do
    it "Example 1" $ mapBinTree (: []) tree3 `shouldBe` Bin (Bin Empty "x" Empty) "a" (Bin Empty "y" Empty)
    it "Example 2" $ mapBinTree toUpper tree4 `shouldBe` Bin (Bin (Bin Empty 'X' Empty) 'A' (Bin Empty 'Y' Empty)) 'B' (Bin Empty 'X' Empty)
    it "Example 3" $ mapBinTree (const 3) tree5 `shouldBe` Bin Empty 3 (Bin (Bin (Bin Empty 3 Empty) 3 (Bin Empty 3 Empty)) 3 (Bin Empty 3 Empty))
    it "Example 4" $ mapBinTree (+1) Empty `shouldBe` Empty
  describe "C-9" $ do
    it "Example 1" $ labelTree Empty `shouldBe` Empty
    it "Example 2" $ labelTree tree1 `shouldBe` Bin Empty ('x',1) Empty
    it "Example 3" $ labelTree tree4 `shouldBe` Bin (Bin (Bin Empty ('x',1) Empty) ('a',2) (Bin Empty ('y',3) Empty)) ('b',4) (Bin Empty ('x',5) Empty)
    it "Example 4" $ labelTree tree6 `shouldBe` Bin (Bin Empty ('c',1) (Bin (Bin (Bin Empty ('x',2) Empty) ('a',3) (Bin Empty ('y',4) Empty)) ('b',5) (Bin Empty ('x',6) Empty))) ('d',7) (Bin (Bin (Bin Empty ('x',8) Empty) ('a',9) (Bin Empty ('y',10) Empty)) ('b',11) (Bin Empty ('x',12) Empty))
