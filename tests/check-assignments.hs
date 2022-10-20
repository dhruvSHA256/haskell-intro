{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Attoparsec.Text ()
import Data.Attoparsec.Text (Parser, string, parse, maybeResult, space, choice)
import Data.Functor (($>))
import Control.Applicative ((<|>), many, (<**>))
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.Function ((&))
import System.Directory (createDirectoryIfMissing)
import Data.List (intercalate)
import System.Directory.Internal.Prelude (exitFailure, unless)
import qualified Options.Applicative as Opts

import SimpleCmd

import TestsCommon
import Control.Monad (when)

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

  testEntries AssignmentA = ["tests/examples/ATest.hs", "tests/complete/ATest.hs"]
  testEntries AssignmentB = ["tests/complete/BTest.hs"]
  testEntries AssignmentC = ["tests/examples/CTest.hs", "tests/complete/CTest.hs"]
  testEntries AssignmentD = ["tests/complete/DTest.hs"]
  testEntries AssignmentE = ["tests/complete/ETest.hs"]
  testEntries AssignmentF = ["tests/complete/FTest.hs"]


data CLIOpts = CLIOpts { checkOnly :: Bool, allowMany :: Bool, checkRebased :: Bool }

cliOpts :: Opts.Parser CLIOpts
cliOpts = CLIOpts 
  <$> Opts.switch ( Opts.long "check-only" <> Opts.help "Only check submission, don't run test" )
  <*> Opts.switch ( Opts.long "allow-many" <> Opts.help "Allow submission of more than one assignment" )
  <*> Opts.switch ( Opts.long "check-rebased" <> Opts.help "Check if branch is rebased on top of main branch" )

checkError :: Bool -> IO ()
checkError True = pure ()
checkError False = exitFailure 

main :: IO ()
main = do
  opts <- Opts.execParser $ Opts.info (cliOpts <**> Opts.helper) Opts.fullDesc
  createDirectoryIfMissing True "spec-out"
  when (checkRebased opts) checkRebasedOnMaster
  submitted <- listSubmitted @Assignment
  unless (allowMany opts) $ checkOneSubmitted submitted
  unless (checkOnly opts) $ mapM runTest submitted >>= checkError . and
