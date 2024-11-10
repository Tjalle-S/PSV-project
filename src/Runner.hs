{-# LANGUAGE FlexibleContexts #-}

module Runner (run) where

import GCLParser.GCLDatatype (Program)
import Cli( ArgData(..), pruneInfeasible, HeuristicOptions(checkInvariant) )
import WLP (prunedCalcWLP)
import TreeBuilder (progToExecMaxDepth)
import Util (runV, Log, VState)
import Z3Instance ()
import Control.Monad.IO.Class (MonadIO (..))

run :: MonadIO m => ArgData -> Program -> m (Bool, VState, Log)
run args prog = liftIO $ runV
  args
  (prunedCalcWLP
    (pruneInfeasible (enabledHeuristics args)) $
    progToExecMaxDepth
      (checkInvariant (enabledHeuristics args))
      (maxLength args) prog)
