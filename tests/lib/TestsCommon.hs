{-# LANGUAGE OverloadedStrings #-}
module TestsCommon where

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text (Parser, string, parse, maybeResult, space, choice, takeWhile, parseOnly)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Shelly
import Control.Applicative (many)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Data.Functor (($>))
import Text.XML.JUnit (writeXmlReport, inSuite, failed, passed, failureMessage, stdout, failureStackTrace)
import Data.Function ((&))
import System.Exit (exitFailure)
import Control.Monad (filterM)
import System.Directory (doesFileExist)

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
  diffs <- shelly . silently $
    run "git" ["--no-pager", "diff", "--stat", "main", "assignments/"]
  pure . mapMaybe (hush . parseOnly (takeWhile isSpace *> pAssignment)) . T.lines $ diffs

-- | Executes `cabal exec runghc --` with provided options. 
runghc :: [Text] -> IO ()
runghc cmd = shelly . run_ "cabal" $ ["exec", "runghc", "--"] <> cmd

runTest :: IsAssignment a => a -> IO ()
runTest a = filterM doesFileExist (testEntries a) >>= mapM_ (runghc . cmd)
  where
    cmd s = [T.pack s]


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
  log <- shelly $ run "git" ["--no-pager", "log", "--oneline", "..main"]
  let n = length $ T.lines log
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
