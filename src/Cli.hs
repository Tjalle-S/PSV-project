module Cli (getOptions, ArgData(..), Command(..), HeuristicOptions(..)) where

import Options.Applicative

data Command = Version | Args ArgData

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
}

-- | Parser for commandline options.
parseOptions :: Parser Command
parseOptions =  Args <$> (ArgData 
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
  <*> parseHeuristics)
  <|> Version <$ switch (
       long "version"
    <> help "Show the version of GLEE"
  )
  
parseHeuristics :: Parser HeuristicOptions
parseHeuristics = HeuristicOptions
  <$> option auto (
       long "prune-infeasible"
    <> short 'p'
    <> metavar "int"
    <> help "Attempt to prune infeasible paths")

-- | Get all commandline options.
getOptions :: IO Command
getOptions = execParser $ info
  (parseOptions <**> helper) $ fullDesc
    <> progDesc "GCL program verifier using wlp-based bounded symbolic execution"
