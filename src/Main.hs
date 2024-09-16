{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Cli
import GCLParser.Parser
import Text.Pretty.Simple
    ( CheckColorTty(NoCheckColorTty),
      OutputOptions(OutputOptions),
      pPrintOpt,
      StringOutputStyle(EscapeNonPrintable) ) 
import Control.Monad.IO.Class (MonadIO)
import GCLParser.PrettyPrint (ppProgram2String)

main :: IO ()
main = do
  ArgData { .. } <- getOptions
  ast <- parseGCLfile fileName
  either (const $ putStrLn $ "Parse error in file " ++ fileName) (putStrLn . ppProgram2String) ast
  -- return ()

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = pPrintOpt NoCheckColorTty (OutputOptions 2 120 True True 0 Nothing EscapeNonPrintable)