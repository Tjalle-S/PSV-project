module Main (main) where

import Test.Tasty.Bench
import GCLParser.GCLDatatype (Program)
import GCLParser.Parser (parseGCLstring)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import Cli (ArgData(..), HeuristicOptions(..))
import Runner (run)
import Data.Bool (bool)

main :: IO ()
main = defaultMain
  [ bgroup "Benchmarks" (makeGroups makeValid $ \n -> bench n . nfIO . getResultOf)
  , bgroup "Test Valid Programs"   (makeGroups makeValid   $ \n -> testCase n . makeAssertion True)
  , bgroup "Test Invalid Programs" (makeGroups makeInvalid $ \n -> testCase n . makeAssertion False)
  ]

-- Make a group of tests or benchmarks, with subgroups per program.
makeGroups :: (String -> String) -> (String -> IO Program -> TestTree) -> [TestTree]
makeGroups g t = [bgroup name (makeGroup name) | name <- programs]
  where
    makeGroup name = [withResource
      (getProgram (g name) n)
      (const $ return ())
      (t $ "N = " ++ show n) | n <- valuesOfN]

-- | Get the result of running the verifier on the given program.
getResultOf :: IO Program -> IO Bool
getResultOf mp = do
  p <- mp
  (res, _, _) <- run defaultArgs p
  return res

-- | Create an assertion for testing the given program.
makeAssertion :: Bool -> IO Program -> Assertion
makeAssertion expected mp = do
  res <- getResultOf mp
  assertEqual ("Expected program to " ++ accOrRej expected ++ " but got " ++ accOrRej res) expected res
  where
    accOrRej = bool "reject" "accept"

defaultArgs :: ArgData
defaultArgs = ArgData {
  fileName            = "" -- Not necessary.
, maxLength           = 50
, showStats           = False
, dumpConditions      = False
, enableAllHeuristics = False
, enabledHeuristics   = HeuristicOptions {
    pruneInfeasible = 50
  , simplifyExpressions = True
  , checkInvariant  = False
}
}

valuesOfN :: [Int]
valuesOfN = [2..4]

makeValid, makeInvalid :: String -> String
makeValid   = ("valid/" ++)
makeInvalid = ("invalid/" ++)

programs :: [String]
programs = ["memberOf", "divByN", "pullUp", "bsort"]

-- | Read a program with the given name, and replace N by the given constant.
getProgram :: String -> Int -> IO Program
getProgram name n = do
  str <- concatMap replace <$> readFile ("benchmarks/" ++ name ++ ".gcl")
  case parseGCLstring str of
    Left  _err -> error "Parse error"
    Right prog -> return prog
  where
    replace 'N' = show n
    replace  c  = [c]