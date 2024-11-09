module Main (main) where

import Test.Tasty.Bench
import GCLParser.GCLDatatype (Program)
import GCLParser.Parser (parseGCLstring)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import Cli (ArgData(..), HeuristicOptions(..))
import Runner (run) 
import Data.Bool (bool)
import Test.Tasty.Options (IsOption(..), safeRead, safeReadBool, OptionDescription (..), lookupOption, setOption)
import Data.Proxy (Proxy (..))
import Test.Tasty.Runners (TestTree (AskOptions), NumThreads (NumThreads), parseOptions, tryIngredients)
import Test.Tasty.Ingredients.ConsoleReporter (MinDurationToReport(MinDurationToReport))
import System.Exit (exitFailure, exitSuccess)

newtype Depth     = Depth Int
newtype Prune     = Prune Int
newtype Invariant = Invariant Bool

instance IsOption Depth where
  defaultValue = Depth (-1)
  parseValue   = fmap Depth . safeRead
  optionName   = pure "depth"
  optionHelp   = pure "The maximum depth up to which to search"

instance IsOption Prune where
  defaultValue = Prune (-1)
  parseValue   = fmap Prune . safeRead
  optionName   = pure "prune"
  optionHelp   = pure "Prune infeasible branches up to a certain depth"

instance IsOption Invariant where
  defaultValue = Invariant False
  parseValue   = fmap Invariant . safeReadBool
  optionName   = pure "invariant"
  optionHelp   = pure "Detect annotated loop invariants"

main :: IO ()
main = do
  let customOpts  = [Option (Proxy :: Proxy Depth), Option (Proxy :: Proxy Prune), Option (Proxy :: Proxy Invariant)]
  let ingredients = includingOptions customOpts : benchIngredients
  opts <- parseOptions ingredients tests
  let opts' = setOption (MinDurationToReport 9999999999999999) $ setOption (NumThreads 1) opts
  case tryIngredients ingredients opts' tests of
    Nothing -> exitFailure
    Just act -> act >>= \x -> if x then exitSuccess else exitFailure
  
tests :: TestTree
tests = AskOptions $ \opts -> 
  let d           = let Depth d' = lookupOption opts in if d' == -1 then maxLength defaultArgs else d'
      Prune     p = lookupOption opts
      Invariant i = lookupOption opts
      args = defaultArgs {
          maxLength = d
        , enabledHeuristics = HeuristicOptions {
            pruneInfeasible = if p == -1 then d else p
            , checkInvariant = i } }
  in  bgroup "All" [ bgroup "Benchmarks" (makeGroups makeValid $ \n -> bench n . nfIO . getResultOf args)
      , bgroup "Test Valid Programs"   (makeGroups makeValid   $ \n -> testCase n . makeAssertion True  args)
      , bgroup "Test Invalid Programs" (makeGroups makeInvalid $ \n -> testCase n . makeAssertion False args)
      ]




  -- defaultMain
  -- [ bgroup "Benchmarks" (makeGroups makeValid $ \n -> bench n . nfIO . getResultOf)
  -- , bgroup "Test Valid Programs"   (makeGroups makeValid   $ \n -> testCase n . makeAssertion True)
  -- , bgroup "Test Invalid Programs" (makeGroups makeInvalid $ \n -> testCase n . makeAssertion False)
  -- ]

-- Make a group of tests or benchmarks, with subgroups per program.
makeGroups :: (String -> String) -> (String -> IO Program -> TestTree) -> [TestTree]
makeGroups g t = [bgroup name (makeGroup name) | name <- programs]
  where
    makeGroup name = [withResource
      (getProgram (g name) n)
      (const $ return ())
      (t $ "N = " ++ show n) | n <- valuesOfN]

-- | Get the result of running the verifier on the given program.
getResultOf :: ArgData -> IO Program -> IO Bool
getResultOf args mp = do
  p <- mp
  (res, _, _) <- run args p
  return res

-- | Create an assertion for testing the given program.
makeAssertion :: Bool -> ArgData -> IO Program -> Assertion
makeAssertion expected args mp = do
  res <- getResultOf args mp
  assertEqual ("Expected program to " ++ accOrRej expected ++ " but got " ++ accOrRej res) expected res
  where
    accOrRej = bool "reject" "accept"

defaultArgs :: ArgData
defaultArgs = ArgData {
  fileName            = "" -- Not necessary.
, maxLength           = 50
, showStats           = False
, dumpConditions      = False
, enabledHeuristics   = HeuristicOptions {
    pruneInfeasible = 50
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