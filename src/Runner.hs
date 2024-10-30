{-# LANGUAGE FlexibleContexts #-}

module Runner (run) where

import GCLParser.GCLDatatype (Program)
import Cli (ArgData (..), pruneInfeasible)
import WLP (prunedCalcWLP)
import TreeBuilder (progToExecMaxDepth)
import Util (runV, ReaderData (..), Log, VState)
import Z3Instance ()
import Control.Monad.IO.Class (MonadIO (..))

run :: MonadIO m => ArgData -> Program -> m (Bool, VState, Log)
run args prog =
  liftIO $ runV (ReaderData args) (prunedCalcWLP (pruneInfeasible (enabledHeuristics args)) $ progToExecMaxDepth (maxLength args) prog)
