{-# LANGUAGE FlexibleContexts #-}

module Runner (run) where

import GCLParser.GCLDatatype (Program)
import Cli (ArgData (..), HeuristicOptions (..))
import WLP (prunedCalcWLP)
import TreeBuilder (progToExecMaxDepth, simplifyTree)
import Util (runV, ReaderData (..), Log, VState, applyWhen)
import Z3Instance ()
import Control.Monad.IO.Class (MonadIO (..))

run :: MonadIO m => ArgData -> Program -> m (Bool, VState, Log)
run args prog = 
  liftIO $ runV (ReaderData args) (prunedCalcWLP (pruneInfeasible (enabledHeuristics args)) $ applyWhen (simplifyExpressions (enabledHeuristics args)) simplifyTree (progToExecMaxDepth (maxLength args) prog))
