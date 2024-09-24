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
import Util
import Data.Bool (bool)
import Control.Monad (when)
import Data.Time (getCurrentTime, diffUTCTime)
-- import Data.Map (empty)

main :: IO ()
main = do
  args@ArgData{ .. } <- getOptions
  ast <- parseGCLfile fileName
  startTime <- getCurrentTime
  (res, st, logs) <- runV (ReaderData args) placeholderVerify
  putStrLn $ bool "reject" "accept" res
  when showStats (print $ stats st)
  endTime <- getCurrentTime
  let timeUsed = diffUTCTime endTime startTime
  putStrLn $ "Total time used: " ++ show timeUsed
  print logs
  -- either (const $ putStrLn $ "Parse error in file " ++ fileName) (putStrLn . ppProgram2String) ast
  -- return ()

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = pPrintOpt NoCheckColorTty (OutputOptions 2 120 True True 0 Nothing EscapeNonPrintable)

placeholderVerify :: V Bool
placeholderVerify = return True
