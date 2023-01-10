module Main where

import Control.Monad
import System.IO
import System.Random
import Text.Read

-- This file contains a somewhat larger task to implement the Mastermind board
-- game in Haskell. This is based on a task that I designed for the functional
-- programming course at Utrecht University in the Netherlands a long time ago.
--
-- For general information on Mastermind, see Wikipedia:
--
-- https://en.wikipedia.org/wiki/Mastermind_(board_game)

-- Overview
-- ========
--
-- Mastermind is a game for two players, called the *codemaker* and the
-- *codebreaker*. The codemaker's role is played by the computer in our case.
-- The codemaker devises a code made up of four positions, each position being
-- one of six colors. For simplicity of interaction, the colors are represented
-- as numbers. The codebreaker (played by the human user of your program)
-- must guess the code in as few turns as possible. After each guess, two scores
-- are determined for the guess.
--
-- The *black* score says how many positions of your code match the solution.
-- Once the black score is 4, the codebreaker has determined the correct code
-- and the game is over. The white score indicates that a position of the
-- codebreaker's code used a color contained in the solution, but in the wrong
-- position. It is easy to see that the sum of black and white score never
-- exceeds 4, the number of positions in the code.
--
-- Here are two logs of possible games:
--
-- Game 1, code 3 4 6 6
--
-- Guess 1:  1 1 2 2   score: 0 black, 0 white
-- Guess 2:  3 3 4 4   score: 1 black, 1 white
-- Guess 3:  3 5 3 6   score: 2 black, 0 white
-- Guess 4:  3 4 6 6   score: 4 black, 0 white
--
--
-- Game 2, code 5 1 1 4
--
-- Guess 1:  1 2 3 4   score: 1 black, 1 white
-- Guess 2:  1 3 5 6   score: 0 black, 2 white
-- Guess 3:  5 2 1 5   score: 2 black, 0 white
-- Guess 4:  5 2 4 1   score: 1 black, 2 white
-- Guess 5:  5 4 1 1   score: 2 black, 2 white
-- Guess 6:  5 1 1 4   score: 4 black, 0 white
--
-- If the game is played as an interactive Haskell program, it coud look at
-- follows:
--
-- ? 2 2 3 3
-- 0 black, 1 white
-- ? 4 4 5 5
-- 0 black, 1 white
-- ? 5 2 1 6
-- 1 black, 3 white
-- ? 5 1 6 2
-- 4 black, 0 white
-- Congratulations.
--
-- The game interactively prompts the user to type in a guess using a question
-- mark as a prompt. The user types in a whitespace-separated sequence of four
-- numbers between 1 and 6, and the game responds with the score.
--
-- If a black score of 4 is reached, the game stops; otherwise, it asks for
-- another guess.


-- PART I
-- ------
--
-- General helper functions for scoring. These will prove useful later
-- when computing the white scores. But these functions are more general
-- and could be used for other purposes as well.
--
-- Task 1.
--
-- Define a function 'extract' that looks for an element in a list.
-- If the element is found, one occurrence of that element is removed
-- and the rest of the list returned. If the element is not found,
-- 'Nothing' is returned.
--
-- For many of the functions in this module, I've created unit tests.
-- These test cases are starting with 'prop_' and should evaluate to
-- 'True' once the function is implemented correctly. You can also
-- look at the test cases to better understand what the function should
-- be doing.

extract :: Eq a => a -> [a] -> Maybe [a]
extract = error "TODO"

-- | Property of 'extract': Successfully find an element.
prop_extract1 :: Bool
prop_extract1 = extract (3 :: Int) [1,2,3,4] == Just [1,2,4]

-- | Property of 'extract': Only remove the first occurrence of an element.
prop_extract2 :: Bool
prop_extract2 = extract (3 :: Int) [1,2,3,3,4] == Just [1,2,3,4]

-- | Property of 'extract': Return 'Nothing' is the element is not found.
prop_extract3 :: Bool
prop_extract3 = extract (3 :: Int) [1,2,4] == Nothing

-- | Property of 'extract': Nothing is contained in the empty list.
prop_extract4 :: Bool
prop_extract4 = extract (3 :: Int) [] == Nothing

-- | Property of 'extract': Should work on any type supporting equality.
prop_extract5 :: Bool
prop_extract5 = extract 'h' "hello" == Just "ello"

-- | Checks all properties of 'extract' at once.
props_extract :: Bool
props_extract =
  and
    [ prop_extract1
    , prop_extract2
    , prop_extract3
    , prop_extract4
    , prop_extract5
    ]

-- Task 2.
--
-- Define a function 'matches' that counts the number of simultaneously
-- matching elements between two lists. An element in one list matches an
-- element in the other list if the two elements are equal. Simultaneous
-- matches cannot involve the same list elements more than once.
--
-- Again, look at the test cases for examples.

matches :: Eq a => [a] -> [a] -> Int
matches = error "TODO"

-- | Property of 'matches': The 5 cannot simultaneously match both 5s in the other list.
prop_matches1 :: Bool
prop_matches1 = matches ([5] :: [Int]) [5,5] == 1

-- | Property of 'matches': Completely disjoint lists contain no matches.
prop_matches2 :: Bool
prop_matches2 = matches ([1,2,3] :: [Int]) [4,5,6] == 0

-- | Property of 'matches': Equal lists contain as many matches as the length of the list.
prop_matches3 :: Bool
prop_matches3 = matches ([1,2,3] :: [Int]) [1,2,3] == 3

-- | Property of 'matches': If two lists are permutations of each other, they still have
-- as many matches as the length of the list.
prop_matches4 :: Bool
prop_matches4 = matches "hello" "olelh" == 5

-- | Property of 'matches': A more interesting example. Three 'b's and a 'c' and a 'd' match,
-- for a total of five matches.
prop_matches5 :: Bool
prop_matches5 = matches "abbcdbdg" "bbebbfcd" == 5

-- | Checks all properties of 'matches' at once.
props_matches :: Bool
props_matches =
  and
    [ prop_matches1
    , prop_matches2
    , prop_matches3
    , prop_matches4
    , prop_matches5
    ]

-- PART II
-- -------
--
-- Colours and guesses. Here, we define new datatypes that are specific to the game.
-- Even though we could just reuse existing types, it is more precise and safer to
-- introduce new datatypes.
--
-- We also define two global constants, the number of colours in use (6) and the size
-- of a code (4 colours). We do not make them variable, but at the same time we should
-- write the entire rest of the code so that these constants could easily be changed,
-- and everything else would still work. So use the constants, not the hard-coded
-- numbers 6 and 4, wherever appropriate.

numberOfColours :: Int
numberOfColours = 6

codeSize :: Int
codeSize = 4


-- Task 3.
--
-- The datatype 'Colour' has the same representation as an 'Int'. The newtype construct
-- used below is almost exactly like 'data', but can only be used in the special case
-- that a datatype has exactly one constructor, with exactly one argument. Note that
-- we reuse the datatype name as the constructor name, which is allowed and common in
-- Haskell. So you can use the function
--
--   Colour :: Int -> Colour
--
-- to change an 'Int' into a 'Colour', and you can use pattern matching as in
--
--   fun (Colour colour) = ...
--
-- to match on an incoming value of type 'Colour' and extract the contained number
-- 'colour' of type 'Int'.
--
-- Define a function 'validColour' that checks whether the 'Int' contained within a
-- 'Colour' is within the valid range, that is, between 1 and 'numberOfColours'.
-- It should be an invariant for the whole program that outside of test cases, we
-- never return 'Colour' values that are invalid.

newtype Colour = Colour Int
  deriving (Eq, Show)

validColour :: Colour -> Bool
validColour = error "TODO"

-- | Property of 'validColour': A negative number is not a valid colour.
prop_validColour1 :: Bool
prop_validColour1 = not (validColour (Colour (-1)))

-- | Property of 'validColour': Zero is not a valid colour.
prop_validColour2 :: Bool
prop_validColour2 = not (validColour (Colour 0))

-- | Property of 'validColour': All numbers between 1 and 'numberOfColours'
-- yield valid colours.
prop_validColour3 :: Bool
prop_validColour3 = all validColour (map Colour [1 .. numberOfColours])

-- | Property of 'validColour': A number that is larger than 'numberOfColours'
-- does not yield a valid colour.
prop_validColour4 :: Bool
prop_validColour4 = not (validColour (Colour (numberOfColours + 1)))

-- | Checks all properties of 'validColour' at once.
props_validColour :: Bool
props_validColour =
  and
    [ prop_validColour1
    , prop_validColour2
    , prop_validColour3
    , prop_validColour4
    ]


-- Task 4.
--
-- The datatype 'Code' represents a code, that is, a sequence of colours that
-- could either be the correct solution the codemaker has invented, or a guess
-- that the codebreaker has made. A 'Code' consists always of 'codeSize' many
-- elements.
--
-- Define a function 'validCode' that checks this property, i.e., it should
-- verify that the list has exactly the correct length.
-- It should be an invariant for the whole program that outside of test cases, we
-- never return 'Code' values that are invalid.
--
-- (We could recursively check whether the colours are valid here as well,
-- but as whatever produced the colours should already have ensured the colours
-- themselves are valid, we choose not to.)

newtype Code = Code [Colour]
  deriving (Eq, Show)

validCode :: Code -> Bool
validCode = error "TODO"

-- | Property of 'validCode': A code of 'codeSize' items is valid.
prop_validCode1 :: Bool
prop_validCode1 = validCode (Code (replicate codeSize (Colour 1)))

-- | Property of 'validCode': A code that is too long is invalid.
prop_validCode2 :: Bool
prop_validCode2 = not (validCode (Code (replicate (codeSize + 1) (Colour 1))))

-- | Property of 'validCode': A code that is too short is invalid (except
-- is 'codeSize' is set to 0).
prop_validCode3 :: Bool
prop_validCode3 = codeSize <= 0 || not (validCode (Code (replicate (codeSize - 1) (Colour 1))))

-- | Checks all properties of 'validCode' at once.
props_validCode :: Bool
props_validCode =
  and
    [ prop_validCode1
    , prop_validCode2
    , prop_validCode3
    ]


-- PART III
-- --------
--
-- Scoring. In this part, we define the functions to compute the black and
-- white score of a guess when compared against a solution. We will make use
-- of the helper functions defined in Part I here.

-- Task 5.
--
-- We define a type synonym for 'Score' to be treated the same as 'Int'. So
-- unlike 'data' or 'newtype', this does not create a new distinct datatype,
-- but just a synonym. No explicit conversion between 'Score' and 'Int' is
-- needed.
--
-- Define a function 'blackScore' that computes the black score for a guess
-- with respect to the solution. (It does not matter which of the two arguments
-- is which, the function is commutative.) See the introduction for more
-- information on how the black score is computed. You can use explicit recursion
-- or existing library functions here. The helper functions from Part I do *not*
-- help here, they're going to help when computing the white score ...

type Score = Int

blackScore :: Code -> Code -> Score
blackScore = error "TODO"

-- | Property of 'blackScore': only the 4 is in the correct position.
prop_blackScore1 :: Bool
prop_blackScore1 =
  blackScore
    (Code [Colour 5, Colour 1, Colour 1, Colour 4])
    (Code [Colour 1, Colour 2, Colour 3, Colour 4])
  == 1

-- | Property of 'blackScore': the initial 3 and the final 6 are in the correct positions.
prop_blackScore2 :: Bool
prop_blackScore2 =
  blackScore
    (Code [Colour 3, Colour 4, Colour 6, Colour 6])
    (Code [Colour 3, Colour 5, Colour 3, Colour 6])
  == 2

-- | Property of 'blackScore': nothing is in the correct position.
prop_blackScore3 :: Bool
prop_blackScore3 =
  blackScore
    (Code [Colour 1, Colour 2, Colour 3, Colour 4])
    (Code [Colour 2, Colour 3, Colour 4, Colour 1])
  == 0

-- | Property of 'blackScore': everything is in the correct position.
prop_blackScore4 :: Bool
prop_blackScore4 =
  blackScore
    (Code [Colour 3, Colour 5, Colour 4, Colour 4])
    (Code [Colour 3, Colour 5, Colour 4, Colour 4])
  == 4

-- | Checks all properties of 'blackScore' at once.
props_blackScore :: Bool
props_blackScore =
  and
    [ prop_blackScore1
    , prop_blackScore2
    , prop_blackScore3
    , prop_blackScore4
    ]

-- Task 6.
--
-- Define a function 'whiteScore' that computes the white score for a guess with
-- respect to the solution. (It does not matter which of the two arguments is
-- which, the function is commutative.) See the introduction for more information
-- on how the black score is computed.
--
-- Consider the function 'matches' that we defined in Part I. It computes almost
-- the right count, only that it will count items that are in the correct position
-- as well. So we can define 'whiteScore' to compute the 'matches' between the
-- two lists and subtract the 'blackScore'!

whiteScore :: Code -> Code -> Score
whiteScore = error "TODO"

-- | Property of 'whiteScore': one of the 1s is in the wrong position (the other is unmatched).
prop_whiteScore1 :: Bool
prop_whiteScore1 =
  whiteScore
    (Code [Colour 5, Colour 1, Colour 1, Colour 4])
    (Code [Colour 1, Colour 2, Colour 3, Colour 4])
  == 1

-- | Property of 'whiteScore': nothing is in the wrong position.
prop_whiteScore2 :: Bool
prop_whiteScore2 =
  whiteScore
    (Code [Colour 3, Colour 4, Colour 6, Colour 6])
    (Code [Colour 3, Colour 5, Colour 3, Colour 6])
  == 0

-- | Property of 'whiteScore': everything is in the wrong position.
prop_whiteScore3 :: Bool
prop_whiteScore3 =
  whiteScore
    (Code [Colour 1, Colour 2, Colour 3, Colour 4])
    (Code [Colour 2, Colour 3, Colour 4, Colour 1])
  == 4

-- | Property of 'whiteScore': nothing is in the wrong position.
prop_whiteScore4 :: Bool
prop_whiteScore4 =
  whiteScore
    (Code [Colour 3, Colour 5, Colour 4, Colour 4])
    (Code [Colour 3, Colour 5, Colour 4, Colour 4])
  == 0

-- | Checks all properties of 'whiteScore' at once.
props_whiteScore :: Bool
props_whiteScore =
  and
    [ prop_whiteScore1
    , prop_whiteScore2
    , prop_whiteScore3
    , prop_whiteScore4
    ]

-- Task 7.
--
-- For each actual guess, we have to compute both the black score and the
-- white score. We define a new datatype to store both the scores. This has
-- one constructor with two arguments. The black score should come first,
-- the white score second.
--
-- Define a function 'computeScores' that computes the black and the white
-- score for a pair of codes and returns the result as a value of type
-- 'Scores'.

data Scores = Scores Score Score -- black score first, white score second
  deriving (Eq, Show)

computeScores :: Code -> Code -> Scores
computeScores = error "TODO"

-- | Property of 'computeScores': black score first, white score second.
prop_computeScores1 :: Bool
prop_computeScores1 =
  computeScores
    (Code [Colour 1, Colour 2, Colour 3, Colour 4])
    (Code [Colour 4, Colour 2, Colour 1, Colour 5])
  == Scores 1 2

-- | Property of 'computeScores': a correct guess leads to full black score
-- and zero white score.
prop_computeScores2 :: Bool
prop_computeScores2 =
  computeScores
    (Code [Colour 4, Colour 2, Colour 1, Colour 5])
    (Code [Colour 4, Colour 2, Colour 1, Colour 5])
  == Scores 4 0

-- | Checks all properties of 'computeScores' at once.
props_computeScores :: Bool
props_computeScores =
  and
    [ prop_computeScores1
    , prop_computeScores2
    ]

-- Task 8.
--
-- To display scores to the player, define a function that shows a
-- score as a string, in the form
--
--   x black, y white
--
-- where x and y are the integers representing the black and white score,
-- respectively.
--
-- Remember that you can use 'show' to turn 'Int's into 'String's,
-- and that you can use '++' to append strings.

renderScores :: Scores -> String
renderScores = error "TODO"

prop_renderScores1 :: Bool
prop_renderScores1 =
  renderScores (Scores 1 2)
  == "1 black, 2 white"

prop_renderScores2 :: Bool
prop_renderScores2 =
  renderScores (Scores 0 0)
  == "0 black, 0 white"

prop_renderScores3 :: Bool
prop_renderScores3 =
  renderScores (Scores 0 4)
  == "0 black, 4 white"

-- | Checks all properties of 'renderScores' at once.
props_renderScores :: Bool
props_renderScores =
  and
    [ prop_renderScores1
    , prop_renderScores2
    , prop_renderScores3
    ]

-- Task 9.
--
-- The small function 'winning' should check whether a given value of
-- type 'Scores' represents a winning configuration. This is the case
-- if the black score corresponds to the size of the code.

winning :: Scores -> Bool
winning = error "TODO"


-- PART IV
-- -------
--
-- Parsing and validating user input. In this part, we write functions
-- that help turn textual input coming from the player into values of
-- the correct datatypes.

-- Task 10.
--
-- To parse colours, we have to try to parse the given 'String' as a
-- number. Use 'readMaybe' for that; we do *not* want to crash on invalid
-- input! Then, reuse 'validColour' to check if the resulting colour
-- would be valid. If so, return it, otherwise return 'Nothing'.
--
-- Note that you may be able to use the monadic structure of 'Maybe'
-- here (but you can also just use other functions or constructs to
-- achieve the same).

parseColour :: String -> Maybe Colour
parseColour = error "TODO"

-- | Property of 'parseColour': Unless 'numberOfColours' is too small,
-- the string "3" should parse as a colour.
prop_parseColour1 :: Bool
prop_parseColour1 =
  numberOfColours < 3 || parseColour "3" == Just (Colour 3)

-- | Property of 'parseColour': A character is not a valid input.
prop_parseColour2 :: Bool
prop_parseColour2 =
  parseColour "x" == Nothing

-- | Property of 'parseColour': A "0" is never a valid colour input.
prop_parseColour3 :: Bool
prop_parseColour3 =
  parseColour "0" == Nothing

-- | Property of 'parseColour': Unless 'numberOfColours' is too small,
-- the string "6" should parse as a colour.
prop_parseColour4 :: Bool
prop_parseColour4 =
  numberOfColours < 6 || parseColour "6" == Just (Colour 6)

-- | Property of 'parseColour': A negative number if never a valid
-- colour input.
prop_parseColour5 :: Bool
prop_parseColour5 =
  parseColour "-1" == Nothing

-- | Property of 'parseColour': Unless 'numberOfColours' is large
-- enough, the string "11" should not parse as a colour.
prop_parseColour6 :: Bool
prop_parseColour6 =
  numberOfColours >= 11 || parseColour "11" == Nothing

-- | Checks all properties of 'parseColour' at once.
props_parseColour :: Bool
props_parseColour =
  and
    [ prop_parseColour1
    , prop_parseColour2
    , prop_parseColour3
    , prop_parseColour4
    , prop_parseColour5
    , prop_parseColour6
    ]

-- Task 11.
--
-- To parse a code, we interpret the input as a space-separated
-- sequence of colours.
--
-- Use 'words' to split a string into a list of strings whenever
-- there is whitespace. Then use 'parseColour' to parse the
-- individual strings as colours. Remember that we had discussed
-- a function that allows applying a monadic function such as
-- 'parseColour' to a list conveniently.
--
-- Use 'validCode' to check that the resulting code would be valid,
-- in other words, that the input contained the correct count of
-- colours. If so, return the code, otherwise return 'Nothing'.

parseCode :: String -> Maybe Code
parseCode = error "TODO"

-- | Property of 'parseCode': an example of a correct code input.
prop_parseCode1 :: Bool
prop_parseCode1 =
  codeSize /= 4 || numberOfColours < 4 || parseCode "1 2 3 4" == Just (Code [Colour 1, Colour 2, Colour 3, Colour 4])

-- | Property of 'parseCode': input too short.
prop_parseCode2 :: Bool
prop_parseCode2 =
  codeSize == 3 || parseCode "1 2 3" == Nothing

-- | Property of 'parseCode': input too long.
prop_parseCode3 :: Bool
prop_parseCode3 =
  codeSize == 5 || parseCode "1 2 3 4 5" == Nothing

-- | Property of 'parseCode': invalid colours should lead to invalid input.
prop_parseCode4 :: Bool
prop_parseCode4 =
  parseCode "1 0 3 4" == Nothing

-- | Property of 'parseCode': extra spaces are ok.
prop_parseCode5 :: Bool
prop_parseCode5 =
  codeSize /= 4 || numberOfColours < 4 || parseCode "  4 3   4 3   " == Just (Code [Colour 4, Colour 3, Colour 4, Colour 3])

-- | Checks all properties of 'parseCode' at once.
props_parseCode :: Bool
props_parseCode =
  and
    [ prop_parseCode1
    , prop_parseCode2
    , prop_parseCode3
    , prop_parseCode4
    , prop_parseCode5
    ]

-- PART V
-- ------
--
-- Random generation of colours and guesses.
--
-- The computer has to come up with a solution randomly. This is what
-- we are going to implement in this part.

-- Task 12.
--
-- Define an action 'randomColour' that, when executed, produces a
-- random number in the range of valid colours, i.e., at least 1 and
-- at most 'numberOfColours'.
--
-- You can use the function
--
--   randomRIO :: (Int, Int) -> IO Int
--
-- that takes a range as a pair, and produces an action that when
-- executed provides you with a random number. Remember that you can
-- map over 'IO' actions to change their result type, or you can use
-- 'do' notation to achieve the same.

randomColour :: IO Colour
randomColour = error "TODO"

-- | Property of 'randomColour': a hundred randomly generated colours
-- should all be valid. Note that this property must be an action as
-- well, because it is not deterministic.
prop_randomColour :: IO Bool
prop_randomColour =
  all validColour <$> replicateM 100 randomColour

-- Task 13.
--
-- Define an action 'randomCode' that, when executed, produces a
-- random code of the correct length, i.e., 'codeSize'.
--
-- Remember that we discussed a function that can be used to repeat
-- a certain action, such as 'randomColour' a given number of times.

randomCode :: IO Code
randomCode = error "TODO"

-- | Property of 'randomCode': a hundred randomly generated codes
-- should all be valid.
prop_randomCode :: IO Bool
prop_randomCode =
  all validCode <$> replicateM 100 randomCode


-- PART VI
-- -------
--
-- User interaction. In this final part, we write the functions that
-- actually perform the interaction with the user and put everything
-- together into a game loop.

-- Task 14.
--
-- Reading a single valid code from the user. Complete the action
-- below that prompts with a question mark, then reads a line (the
-- function 'hFlush' ensures that the question mark is really printed
-- to the screen before reading and not buffered). You have to use
-- 'parseCode' on the input and check the result. If the result is
-- 'Nothing', you should print a text such as "Please enter a valid
-- code." and try again with the 'askCode' action. If the result is
-- 'Just', then the 'Code' should be returned.
--
-- Note that you can use 'case' to analyse and pattern match on the
-- result of a function call.

askCode :: IO Code
askCode = do
  putStr "? "
  hFlush stdout
  input <- getLine
  error "TODO"

-- Task 15.
--
-- The function 'game' should perform an entire game loop with the
-- given code as the expected solution.
--
-- In every iteration of the loop, you should use 'askCode' to read
-- a guess, then 'computeScores' to compute the black and white
-- scores, then 'renderScores' to turn them into text output (and
-- 'putStrLn' to print the text to the screen). Finally, you should
-- use 'winning' to check whether you can either stop with a final
-- message such as "Congratulations." or whether you have to continue
-- with another iteration of the loop.

game :: Code -> IO ()
game = error "TODO"

-- Task 16.
--
-- The main program should generate a code using 'randomCode'
-- and then start 'game' with that code.

main :: IO ()
main = error "TODO"

-- Task 17.
--
-- Play the game! If you have done everything correctly, you should
-- also be able to change the constants 'numberOfColours' and 'codeSize'
-- to other reasonable values and everything should continue to work.
--
-- Bonus tasks:
--
-- - Improve the game experience. Count the number of turns the player
-- requires to guess the correct solution and print that number in the
-- end. Allow the user to give up, and in that case, print what the
-- correct solution would have been.
--
-- - Try to implement an algorithm for playing the game. Such algorithms
-- are given on the Wikipedia page. Modify the game so that the computer
-- can play against itself, and print a log of the game.

