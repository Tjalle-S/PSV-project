{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Cli
import GCLParser.Parser
import Text.Pretty.Simple
    ( CheckColorTty(NoCheckColorTty),
      OutputOptions(OutputOptions),
      pPrintOpt,
      StringOutputStyle(EscapeNonPrintable), {-pPrintStringOpt-} ) 
import Control.Monad.IO.Class (MonadIO (..))
import Util
import Control.Monad (when)
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import Runner (run)
import GHC.IsList (IsList(toList))

main :: IO ()
main = do
  -- args@ArgData{ .. } <- return ArgData{fileName="infeasible.gcl", maxLength=10, showStats=True, dumpConditions=True, enableAllHeuristics=True, enabledHeuristics=HeuristicOptions{pruneInfeasible=True}}
  args@ArgData{ .. } <- getOptions
  ast <- parseGCLfile fileName
  (st, timeUsed) <- withTimer $ do
    (_, st, logs) <- case ast of
              Left  _err -> error "Parse error"
              Right prog -> run args prog
    putStr $ unlines $ toList logs
    return st

  when showStats $ do 
    pPrint (stats st)
    putStrLn $ "Total time used: " ++ show timeUsed

printOptions :: OutputOptions
printOptions = OutputOptions 2 120 True True 0 Nothing EscapeNonPrintable

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = pPrintOpt NoCheckColorTty printOptions

-- pPutStrLn :: MonadIO m => String -> m ()
-- pPutStrLn = pPrintStringOpt NoCheckColorTty printOptions

withTimer :: MonadIO m => m a -> m (a, NominalDiffTime)
withTimer ma = do
  startTime <- liftIO getCurrentTime
  a         <- ma
  endTime   <- liftIO getCurrentTime
  return (a, diffUTCTime endTime startTime)

