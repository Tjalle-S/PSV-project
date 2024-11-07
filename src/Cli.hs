module Cli (getOptions, ArgData(..), HeuristicOptions(..)) where

import Options.Applicative

-- | Passed command-line options.
data ArgData = ArgData {
  -- | File name of the file to verify.
  fileName            :: String
  -- | Maximum length of full paths to verify.
, maxLength           :: Int
  -- | Whether or not statistics should be kept track of and printed.
, showStats           :: Bool
  -- | Whether or not all calculated preconditions should be dumped to stdout.
, dumpConditions      :: Bool

  -- | Whether or not heuristics should be enabled.
, enableAllHeuristics :: Bool
  -- | Which heuristics are explicitly enabled.
, enabledHeuristics   :: HeuristicOptions
}

data HeuristicOptions = HeuristicOptions {
  pruneInfeasible :: Int
, checkInvariant  :: Bool
}

-- | Parser for commandline options.
parseOptions :: Parser ArgData
parseOptions =  ArgData 
  <$> argument str (
      metavar "[SOURCE]"
    <> help "GCL file to read"
    <> action "file")
  <*> option auto (
       long "max-length"
    <> short 'k'
    <> metavar "int"
    <> help "Maximum length of paths to verify")
  <*> switch (
       long "show-statistics"
    <> short 's'
    <> help "Keep track of statistics during the process")
  <*> switch (
       long "dump-wlp"
    <> short 'd'
    <> help "Dump all calculated preconditions (even if not evaluated)")
  <*> switch (
       long "optimize"
    <> short 'O'
    <> help "Enable all heuristics")
  <*> parseHeuristics
  
parseHeuristics :: Parser HeuristicOptions
parseHeuristics = HeuristicOptions
  <$> option auto (
       long "prune-infeasible"
    <> short 'p'
    <> value 0
    <> metavar "int"
    <> help "Attempt to prune infeasible paths")
    <*> switch (
       long "check-invariant"
    <> short 'i'
    <> help "Check annotated loop invariants"
  )

-- | Get all commandline options.
getOptions :: IO ArgData
getOptions = execParser $ info
  (parseOptions <**> helper <**> simpleVersioner versionString) $ fullDesc
    <> progDesc "GCL program verifier using wlp-based bounded symbolic execution"

versionString :: String
versionString = "The (Glorious) GCL Logical Execution Engine, version 1.0.0"
