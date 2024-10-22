module Main (main) where

import Test.Tasty.Bench
import GCLParser.GCLDatatype (Program)
import GCLParser.Parser (parseGCLstring)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import Cli (ArgData(..), HeuristicOptions (HeuristicOptions, pruneInfeasible))
import Runner (run)
import Data.Bool (bool)

main :: IO ()
main = defaultMain -- Wall-clock time, since time taken by Z3 would otherwise not be measured.
  [ localOption WallTime $ bgroup "Benchmarks" (makeGroup validPrograms)
  , bgroup "Test Valid Programs"   (makeTests validPrograms   True)
  , bgroup "Test Invalid Programs" (makeTests invalidPrograms False)
  ]

makeGroup :: [String] -> [Benchmark]
makeGroup names = [bgroup name (makeBenches name) | name <- names]

makeBenches :: String -> [Benchmark]
makeBenches name = [withResource
  (getProgram name n)
  (const $ return ())
  (bench ("N = " ++ show n) . benchmarkFull) | n <- valuesOfN]

benchmarkFull :: IO Program -> Benchmarkable
benchmarkFull = nfIO . getResultOf

makeTests :: [String] -> Bool -> [TestTree]
makeTests names expected =
  let n = 7
  in  [withResource
    (getProgram name n)
    (const $ return ())
    (testCase name . testcaseFull expected) | name <- names]

getResultOf :: IO Program -> IO Bool
getResultOf mp = do
  p <- mp
  (res, _, _) <- run defaultArgs p
  return res

testcaseFull :: Bool -> IO Program -> Assertion
testcaseFull expected mp = do
  res <- getResultOf mp
  assertEqual ("Expected program to " ++ accOrRej expected ++ " but got " ++ accOrRej res) expected res
  where
    accOrRej = bool "reject" "accept"

defaultArgs :: ArgData
defaultArgs = ArgData {
  fileName  = "" -- Not necessary.
, maxLength = 65
, showStats = False
, dumpConditions = False
, enableAllHeuristics = False
, enabledHeuristics = HeuristicOptions {
  pruneInfeasible = 0
}
}

valuesOfN :: [Int]
valuesOfN = [2..3]

validPrograms :: [String]
validPrograms = map ("valid/" ++) programs

invalidPrograms :: [String]
invalidPrograms = map ("invalid/" ++) programs

programs :: [String]
programs = ["memberOf", "divByN"{-, "pullUp"-}]

getProgram :: String -> Int -> IO Program
getProgram name n = do
  str <- concatMap replace <$> readFile ("benchmarks/" ++ name ++ ".gcl")
  case parseGCLstring str of
    Left  _err -> error "Parse error"
    Right prog -> return prog
  where
    replace 'N' = show n
    replace  c  = [c]