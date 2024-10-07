module Cli (getOptions, ArgData(..)) where

import Options.Applicative

-- | Passed command-line options.
data ArgData = ArgData {
  -- | File name of the file to verify.
  fileName         :: String
  -- | Maximum length of full paths to verify.
, maxLength        :: Int
  -- | Whether or not statistics should be kept track of and printed.
, showStats        :: Bool
  -- | Whether or not heuristics should be enabled.
, enableHeuristics :: Bool
}

-- | Parser for commandline options.
parseOptions :: Parser ArgData
parseOptions = ArgData 
  <$> argument str (
      metavar "[SOURCE]"
    <> help "GCL file to read")
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
       long "enable-heuristics"
    <> short 'O'
    <> help "Enable all heuristics"
  )

-- | Get all commandline options.
getOptions :: IO ArgData
getOptions = execParser $ info
  (parseOptions <**> helper) $ fullDesc
    <> progDesc "GCL program verifier using wlp-based bounded symbolic execution"
