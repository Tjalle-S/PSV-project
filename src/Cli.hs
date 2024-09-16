module Cli (getOptions, ArgData(..)) where

import Options.Applicative

data ArgData = ArgData {
  fileName :: String
}

parseOptions :: Parser ArgData
parseOptions = ArgData 
  <$> argument str (
      metavar "[TARGET]"
    <> help "Target file to write to")

getOptions :: IO ArgData
getOptions = execParser $ info
  (parseOptions <**> helper) $ fullDesc
    <> progDesc "Official compiler for the Agon language"
