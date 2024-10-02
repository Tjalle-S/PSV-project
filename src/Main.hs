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
import Data.Fix (Fix (..))
--import GCLParser.GCLDatatype

import TreeBuilder --(progToExec,progToExecMaxDepth)
import WLP
import Control.Monad (when)

main :: IO ()
main = do
  ArgData { .. } <- getOptions
  ast <- parseGCLfile fileName
  either (const $ putStrLn $ "Parse error in file " ++ fileName) (pPrint . (flip wlpTree) (Fix $ LitB True) . progToExecMaxDepth 10) ast
  -- return ()

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = pPrintOpt NoCheckColorTty (OutputOptions 2 120 True True 0 Nothing EscapeNonPrintable)