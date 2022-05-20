---------------------------------------------------------------------
-- D.hs
--
-- (c) 2017-2021 Andres Loeh, Well-Typed LLP

{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module D where

import Data.List (sortBy)
import Prelude hiding (head, tail)
import Text.Read (readMaybe)

-- Task D-1:
--
-- Define a function 'classify' that decides whether
-- a given string does in fact parse as an integer.
--
-- Use the function 'readMaybe'.
--
-- Examples:
--
-- >>> classify " 34   "
-- AnInt 34
-- >>> classify " - 1"
-- AnInt (-1)
-- >>> classify "hello"
-- AString "hello"
-- >>> classify "1!!!1"
-- AString "1!!!1"
-- >>> classify "42"
-- AnInt 42
--

data IntOrString =
    AnInt Int
  | AString String
  deriving (Show, Read)

classify :: String -> IntOrString
classify = error "TODO: define classify, using readMaybe"

-- Task D-2.
--
-- Define an 'Eq' instance for IntOrString that
-- deviates from the standard equality in that it
-- considers any two strings equal, but distinguishes
-- between different integers.
--
-- Examples:
--
-- >>> AnInt 3 == AnInt 5
-- False
-- >>> AnInt 4 == AnInt 4
-- True
-- >>> AnInt 7 == AString "foo"
-- False
-- >>> AString "bar" == AnInt (-12)
-- False
-- >>> AString "foo" == AString "bar"
-- True
-- >>> AString "hello" == AString ""
-- True
--

-- TODO: Define Eq instance for IntOrString

-- Task D-3.
--
-- We define non-empty lists to be plain lists that
-- have one guaranteed first element.
--
--   data NonEmpty a = a :| [a]
--
-- The constructor (:|) is an infix constructor, so
-- the type of the constructor is
--
--   (:|) :: a -> [a] -> NonEmpty a
--
-- Define selector functions head and tail on this
-- type. Give the type signatures yourself. Note that
-- unlike for plain lists, these selector functions
-- are total for non-empty lists.
--
-- (This type is available in the base package from
-- the module Data.List.NonEmpty, but we are defining
-- our own version here.)

data NonEmpty a = a :| [a]
  deriving Show

head = error "TODO: define head, give a type signature"
tail = error "TODO: define tail, give a type siganture"

-- Task D-4.
--
-- Define a function 'cons' that prepends a single
-- element to a non-empty list.
--

cons :: a -> NonEmpty a -> NonEmpty a
cons = error "TODO: define cons"

-- Task D-5.
--
-- Define a function 'group' that turns a list of
-- elements into a list of non-empty list (groups)
-- where each list contains adjacent equal elements
-- of the original list.
--
-- Examples:
--
-- >>> group [1,1,0,2,2,2]
-- [1 :| [1],0 :| [],2 :| [2,2]]
-- >>> group "hello"
-- ['h' :| "",'e' :| "",'l' :| "l",'o' :| ""]
-- >>> group "aaaaxxaaaax"
-- ['a' :| "aaa",'x' :| "x",'a' :| "aaa",'x' :| ""]

group :: Eq a => [a] -> [NonEmpty a]
group = error "TODO: define group"

-- Task D-6.
--
-- Without deriving the instance, make NonEmpty an
-- instance of the Eq class, defining equality in the
-- same way as the derived instance would compute it.

-- TODO: Define Eq instance for NonEmpty

-- Task D-7:
--
-- Without deriving the instance, make NonEmpty an
-- instance of the Functor class, which means you will
-- have to define an fmap function for the NonEmpty
-- datatype.

-- TODO: Define Functor instance for NonEmpty

-- Task D-8:
--
-- Derive a Foldable instance for the NonEmpty datatype.
-- For this, you can modify the data declaration above.
--
-- (Note that for this you will have to enable the
-- DeriveFoldable language extension for this module.)
--
-- Convince yourself that functions such as
-- 'toList', 'length', 'sum' and others work correctly.
--
-- What can you say about the 'null' function?
--
-- PLEASE ANSWER THE QUESTIONS HERE

-- Task D-9:
--
-- Use the function 'sortBy' from the Data.List module
-- to define a function 'sortDescending' that sorts a
-- list in descending rather than ascending order, and
-- a function 'sortSnds' that sorts a lists of pairs
-- based on the second components only.
--
-- Example:
--
-- >>> sortDescending [1,7,3,4,6,4,5]
-- [7,6,5,4,4,3,1]
-- >>> sortDescending "hello world"
-- "wroolllhed "
-- >>> sortSnds [(1,7), (3,4), (7,6), (5,2), (2,6), (9,1)]
-- [(9,1),(5,2),(3,4),(7,6),(2,6),(1,7)]
-- >>> sortSnds [(True, 2.5), (False, 4.5), (True, 2.7), (False, 9.234)]
-- [(True,2.5),(True,2.7),(False,4.5),(False,9.234)]
--

sortDescending :: Ord a => [a] -> [a]
sortDescending = error "TODO: define sortDescending using sortBy"

sortSnds :: Ord b => [(a,b)] -> [(a,b)]
sortSnds = error "TODO: define sortSnds using sortBy"

