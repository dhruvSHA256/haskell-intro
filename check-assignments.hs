{-# LANGUAGE OverloadedStrings #-}
module Main where

import Shelly
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Attoparsec.Text ()
import Data.Attoparsec.Text (Parser, string, parse, maybeResult, space, choice)
import Data.Functor (($>))
import Control.Applicative ((<|>), many, (<**>))
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Text.XML.JUnit (writeXmlReport, inSuite, failed, passed, failureMessage, stdout, failureStackTrace)
import Data.Function ((&))
import System.Directory (createDirectoryIfMissing)
import Data.List (intercalate)
import System.Directory.Internal.Prelude (exitFailure)
import qualified Options.Applicative as Opts

class IsAssignment a where
  srcPath :: a -> Text
  testEntry :: a -> Text

data Assignment =
    AssignmentA
  | AssignmentB
  | AssignmentC
  | AssignmentD
  | AssignmentE
  | AssignmentF
  deriving (Bounded, Enum, Eq, Ord, Show)

instance IsAssignment Assignment where
  srcPath AssignmentA = "assignments/a/A.hs"
  srcPath AssignmentB = "assignments/b/B.hs"
  srcPath AssignmentC = "assignments/c/C.hs"
  srcPath AssignmentD = "assignments/d/D.hs"
  srcPath AssignmentE = "assignments/e/E.hs"
  srcPath AssignmentF = "assignments/f/F.hs"

  testEntry AssignmentA = "ATest.hs"
  testEntry AssignmentB = "BTest.hs"
  testEntry AssignmentC = "CTest.hs"
  testEntry AssignmentD = "DTest.hs"
  testEntry AssignmentE = "ETest.hs"
  testEntry AssignmentF = "FTest.hs"

pAssignment :: (Bounded a, Enum a, IsAssignment a) => Parser a
pAssignment = choice $ fmap (\e -> string (srcPath e) $> e) [minBound .. maxBound]

parseDiff :: (Bounded a, Enum a, IsAssignment a) => Text -> Maybe a
parseDiff = maybeResult . parse (many space *> pAssignment)

submitted :: (Bounded a, Enum a, IsAssignment a) => IO [a]
submitted = do
  diffs <- shelly . silently $
    run "git" ["--no-pager", "diff", "--stat", "main", "assignments/"]
  pure . mapMaybe parseDiff . T.lines $ diffs

runGHC :: Text -> IO ()
runGHC suite = shelly $ run_ "cabal" ["exec", "runghc", "--", "tests/" <> suite]

runTest :: IsAssignment a => a -> IO ()
runTest a = runGHC (testEntry a)

checkOne :: (Assignment -> IO ()) -> [Assignment] -> IO ()
checkOne check [] = do
  putStrLn "No submissions detected"
  writeXmlReport "spec-out/checkOne.xml"
    [ failed "Check if one and only one assignment is submitted"
    & failureMessage "No submissions detected."
    & inSuite "check submission" ]
  exitFailure
checkOne check [a] = do
  writeXmlReport "spec-out/checkOne.xml"
    [ passed "Check if one and only one assignment is submitted"
    & inSuite "check submission" ]
  check a
checkOne check as@(_:_) = do
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


data CLIOpts = CLIOpts { checkOnly :: Bool }

cliOpts :: Opts.Parser CLIOpts
cliOpts = CLIOpts <$> Opts.switch ( Opts.long "check-only" <> Opts.help "Only check submission, don't run test" )

main :: IO ()
main = do
  opts <- Opts.execParser $ Opts.info (cliOpts <**> Opts.helper) Opts.fullDesc
  let test = if checkOnly opts then const (pure ()) else runTest
  createDirectoryIfMissing True "spec-out"
  checkRebasedOnMaster
  submitted >>= checkOne test
