---------------------------------------------------------------------
-- F.hs
--
-- (c) 2017-2021 Andres Loeh, Well-Typed LLP

{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module F where

import Control.Monad hiding (foldM)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Char
import qualified Data.Map.Strict as M
import System.IO
import Text.Read (readMaybe)

-- Task F-1.
--
-- Recall function 'mkMonth' from Assignments B, which
-- validates a month and provides a 'Month' value as a
-- witness of successful validation.
--
-- Define
--
--   validateMonths :: [Int] -> Maybe [Month]
--
-- that checks that all the given integers are valid
-- months.
--
-- Use a suitable higher-order function to solve this.
--
-- Examples:
--
-- >>> validateMonths []
-- Just []
-- >>> validateMonths [1,4,7,10]
-- Just [MkMonth 1,MkMonth 4,MkMonth 7,MkMonth 10]
-- >>> validateMonths [1,4,7,13]
-- Nothing
-- >>> validateMonths [1,-2,7,10]
-- Nothing

newtype Month = MkMonth Int
  deriving Show

mkMonth :: Int -> Maybe Month
mkMonth i
  | i >= 1 && i <= 12 = Just (MkMonth i)
  | otherwise         = Nothing

validateMonths :: [Int] -> Maybe [Month]
validateMonths = error "TODO: define validateMonths"

-- Task F-2.
--
-- There is a variant of 'foldl' suitable for use in
-- a monadic setting, called 'foldM':
--
--   foldM :: Monad m => (r -> a -> m r) -> r -> [a] -> m r
--
-- (Let's ignore strictness issues here.)
--
-- The second argument is the initial value of the accumulator,
-- the first argument is an update function for the accumulator
-- based on the current value of the accumulator and the next
-- list element.
--
-- All the monadic actions resulting from the calls to the
-- update function should be put in sequence.
--
-- Examples (includes printed output, which the eval plugin does not):
--
-- >>> foldM (\ a x -> if odd a then Nothing else Just (a + x)) 0 [1..10]
-- Nothing
-- >>> foldM (\ a x -> if odd a then Nothing else Just (a + x)) 0 [2,2,2,2]
-- Just 8
-- >>> foldM (\ a x -> print x >> return (a + 1)) 0 [5,6,7]
-- 5
-- 6
-- 7
-- 3
--

foldM :: Monad m => (r -> a -> m r) -> r -> [a] -> m r
foldM = error "TODO: define foldM"

-- Task F-3.
--
-- For the following datatypes defining the layout of a level
-- through which one can navigate, write an automatic navigation
-- function
--
--   navigate :: Level -> RoomId -> [Dir] -> Maybe RoomId
--
-- that starts at the given room id, follows the path, and returns
-- the final room we end up in. If any operation fails, then the
-- whole function should fail.
--
-- Use the monadic structure of 'Maybe' where appropriate.
--
-- Examples:
--
-- >>> navigate exampleLevel entrance (replicate 10 E)
-- Just (RoomId 6)
-- >>> navigate exampleLevel entrance [E,N,N]
-- Just (RoomId 7)
-- >>> navigate exampleLevel entrance [E,N,S,W,W,W]
-- Nothing
--

type Level = M.Map RoomId Room

newtype RoomId = RoomId Int
  deriving (Eq, Ord, Show)

data Room = Room { roomDescription :: String, roomExits :: M.Map Dir RoomId }
  deriving Show

data Dir = N | E | S | W
  deriving (Eq, Ord, Show, Read)

entrance, deadEnd, crossing, treasureRoom, pit, teleporterRoom, goal :: RoomId
entrance       = RoomId 1
deadEnd        = RoomId 2
crossing       = RoomId 3
treasureRoom   = RoomId 4
pit            = RoomId 5
teleporterRoom = RoomId 6
goal           = RoomId 7

exampleLevel :: Level
exampleLevel =
  M.fromList
    [ (entrance,       Room "Entrance"        (M.fromList [(W, deadEnd), (E, crossing)]))
    , (deadEnd,        Room "Dead End"        (M.fromList [(E, entrance)]))
    , (crossing,       Room "Crossing"        (M.fromList [(W, entrance), (N, treasureRoom), (S, pit), (E, teleporterRoom)]))
    , (treasureRoom,   Room "Treasure Room"   (M.fromList [(N, goal), (S, crossing)]))
    , (pit,            Room "Pit"             (M.fromList []))
    , (teleporterRoom, Room "Teleporter Room" (M.fromList [(W, crossing), (N, deadEnd), (S, entrance), (E, teleporterRoom)]))
    , (goal,           Room "Goal"            (M.fromList [(S, treasureRoom)]))
    ]

navigate :: Level -> RoomId -> [Dir] -> Maybe RoomId
navigate = error "TODO: define navigate"

-- Task F-4.
--
-- Define a function 'play' that allows you to play a level interactively.
-- Print suitable error messages in error situations, allow the player to
-- continue if it is a error based on user input, end the program with an
-- appropriate message if the player reaches the Pit (pointer with no way out).
--
-- You can use the function 'parseDir' given below.

parseDir :: IO Dir
parseDir = do
  putStr "? "
  hFlush stdout
  c <- getChar
  putStrLn ""
  case readMaybe [toUpper c] of
    Just dir -> return dir
    Nothing  -> do
      putStrLn "Invalid direction"
      parseDir

play :: Level -> RoomId -> IO ()
play =
  error "TODO: define play"

-- Task F-5.
--
-- The following datatypes define a very simplistic imperative
-- language. Variables can be assigned values. Expressions can
-- refer to the current value of variables. Assignments are
-- executed in sequence.
--
-- Uninitialised variables are assumed to be 0!
--
-- Using the state monad, define functions 'evalExpr' and 'evalProgram'
-- that can evaluate a program maintaining an environment (which
-- stores the current values of variables).
--
-- The wrapper function 'runProgram' is given and should return
-- the final integer value of a program.
--
-- Examples:
--
-- >>> runProgram (Return (Var "x"))
-- 0
-- >>> runProgram (Assign "x" (Lit 3) $ Assign "x" (Add (Var "x") (Lit 1)) $ Return (Var "x"))
-- 4
-- >>> runProgram (Assign "x" (Lit 3) $ Assign "y" (Add (Var "x") (Var "x")) $ Assign "x" (Add (Var "x") (Var "y")) $ Return (Var "x"))
-- 9

data Program =
    Assign String Expr Program
  | Return Expr
  deriving Show

data Expr =
    Var String
  | Lit Int
  | Add Expr Expr
  deriving Show

type Env = M.Map String Int

runProgram :: Program -> Int
runProgram p =
  evalState (evalProgram p) M.empty

evalProgram :: Program -> State Env Int
evalProgram = error "TODO: define evalProgram"

evalExpr :: Expr -> State Env Int
evalExpr = error "TODO: define evalExpr"

