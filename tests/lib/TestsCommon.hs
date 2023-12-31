{-# LANGUAGE OverloadedStrings #-}
module TestsCommon where

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text (Parser, string, parse, maybeResult, space, choice, takeWhile, parseOnly)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative (many)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe, isJust)
import Data.Functor (($>))
import Data.List ( isInfixOf, dropWhileEnd )
import Text.XML.JUnit (writeXmlReport, inSuite, failed, passed, failureMessage, stdout, failureStackTrace)
import Data.Function ((&))
import System.Exit (exitFailure)
import Control.Monad ( filterM, void )
import System.Directory ( doesFileExist, getCurrentDirectory )
import Test.Hspec

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)

import System.Environment (getExecutablePath)
import System.FilePath (dropExtension, (</>), takeDirectory)

import SimpleCmd (cmd_, cmd, cmdBool, cmdFull, cmdMaybe, cmdSilent, cmdStdIn)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Class (lift)

second :: Int
second = 10 ^ (6 :: Int)

-- | Types that represent assignments.
-- Constraints reflect the fact that we expect to have
-- finite number of assignments.
class (Bounded a, Enum a) => IsAssignment a where
  srcPath :: a -> Text
  testEntries :: a -> [FilePath]

-- | Parse path and identify if that is path to the assignment source file.
pAssignment :: IsAssignment a => Parser a
pAssignment = choice $ fmap (\e -> string (srcPath e) $> e) [minBound .. maxBound]

-- | Lists submitted assignments
listSubmitted :: IsAssignment a => IO [a]
listSubmitted = do
  diffs <- cmd "git" ["--no-pager", "diff", "--stat", "main...", "assignments/"]
  pure . mapMaybe (hush . parseOnly (takeWhile isSpace *> pAssignment) . T.pack) . lines $ diffs

-- | Executes `cabal exec runghc --` with provided options. 
runghc :: [String] -> IO Bool
runghc c = cmdBool "cabal" $ ["exec", "runghc", "--"] <> c

buildAndRun :: String -> IO Bool
buildAndRun c = do
  cmdSilent "cabal" ["exec", "ghc", "--", c]
  cmdBool (dropExtension c) []

runTest :: IsAssignment a => a -> IO Bool
runTest a = filterM doesFileExist (testEntries a) >>= mapM buildAndRun >>= pure . and


hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush (Left _) = Nothing

-- | If more than one submission is detected, this
-- terminates execution (with 'exitFailure') and writes
-- failed test to @spec-out/checkOne.xml@ (to be used in CI).
checkOneSubmitted :: IsAssignment a => [a] -> IO ()
checkOneSubmitted [] = do
  putStrLn "No submissions detected"
  writeXmlReport "spec-out/checkOne.xml"
    [ failed "Check if one and only one assignment is submitted"
    & failureMessage "No submissions detected."
    & inSuite "check submission" ]
  exitFailure
checkOneSubmitted [a] =
  writeXmlReport "spec-out/checkOne.xml"
    [ passed "Check if one and only one assignment is submitted"
    & inSuite "check submission" ]
checkOneSubmitted as@(_:_) = do
  let slist = T.unlines $ fmap (\a -> "  - " <> srcPath a) as
  T.putStrLn ("Multiple assignments detected:\n" <> slist )
  writeXmlReport "spec-out/checkOne.xml"
    [ failed "Check if one and only one assignment is submitted"
    & failureStackTrace [
        "Too many assignments submitted: ", slist,
        "I assume you wanted to submit " <> srcPath (last as) <> ". If so, use:",
        "  git checkout main -- " <> T.intercalate " " (fmap srcPath $ init as),
        "  git commit -m \"remove previous submissions\"",
        "  git push",
        "to fix this issue."
      ]
    & inSuite "check submission" ]
  exitFailure

-- | Fails if current branch is not rebased on main branch.
checkRebasedOnMaster :: IO ()
checkRebasedOnMaster = do
  log <- cmd "git" ["--no-pager", "log", "--oneline", "..main"]
  let n = length $ lines log
  if n > 0
     then do
      writeXmlReport "spec-out/git.xml"
        [ failed "Check if branch is rebased on main"
        & failureMessage ("There are " <> T.pack (show n) <> " commits on main branch.")
        & failureStackTrace ["To fix use:", "  git fetch origin main:main", "  git merge main", "  git push"]
        & inSuite "check git" ]
      exitFailure
     else
      writeXmlReport "spec-out/git.xml"
        [ passed "Check if branch is rebased on main" & inSuite "check git" ]

getExecutableDir :: IO FilePath
getExecutableDir = takeDirectory <$> getExecutablePath

-- | Checks if solution uses constant memory.
-- Argument is a Haskell source file which is compiled
-- and then run with '-M5m' option. 
-- It expects that program completes successfully.
--
-- The 'src' argument is assumed to be a path to program that
-- should be compiled and run relative to the executable
-- path of the executable that calls this function.
--
-- We need that because we use @cabal exec ghc@ to compile
-- 'src', so we cannot change the work dir. 
runsInConstantMemory :: FilePath -> Expectation
runsInConstantMemory src = do
  dir <- getExecutableDir
  result <- timeOut 3 (test dir)
  case result of
    Right (Right True) -> pure ()
    Right (Right False) -> expectationFailure "Process exited with non-zero code"
    Right (Left _) -> expectationFailure "Heap exhausted: solution does not use constant memory"
    Left () -> expectationFailure "Solution did not finish within 3s"
  where
    test dir = do
      let src' = dir </> src
      cmdSilent "cabal" ["exec", "ghc", "--", "-rtsopts", "-fforce-recomp", src']
      (ecode, _, err) <- cmdFull (dropExtension src') ["+RTS", "-M5m"] ""
      if "Heap exhausted" `isInfixOf` err
         then pure $ Left ()
         else pure $ Right ecode

-- | Run program with stdin as input and verify if output is stdout
-- It timeouts after 3s
--
-- All paths are assumed to be relative to the executable
-- path of the executable that calls this function.
--
-- We need that because we use @cabal exec runhaskell@ to run
-- 'prog', so we cannot change the work dir.
testProg :: FilePath -> [FilePath] -> FilePath -> FilePath -> Expectation
testProg prog args input output = do
  dir <- getExecutableDir
  let test = readFile (dir </> input) >>= cmdStdIn "cabal" params
      params = ["exec", "runhaskell", "--", dir </> prog] <> args

  result <- timeOut 3 test
  case result of
    Right out -> readFile (dir </> output) >>= (trimLines out `shouldBe`)
    Left () -> expectationFailure $ prog <> " did not finish within 3s"
  where
    trimLines = unlines . fmap trim . lines
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Run given IO action with a timeout (in seconds)
timeOut :: Int -> IO a -> IO (Either () a)
timeOut n = race wait
  where
    wait = void $ threadDelay (n * second)

