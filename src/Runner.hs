{-# LANGUAGE FlexibleContexts #-}

module Runner (run) where

import GCLParser.GCLDatatype (Program)
import Control.Monad.Writer (MonadWriter (tell))
import Cli (ArgData (..), pruneInfeasible)
import Expr (Expr(LitB))
import WLP (makeWLPs, calcWLP, prunedCalcWLP)
import TreeBuilder (progToExecMaxDepth)
import Util (runV, ReaderData (..), incrNumPaths, Log, MonadG, VState, whenRs)
import Z3.Monad (MonadZ3)
import Z3Instance ()
import Z3Util (getValidityCounterExample, expr2ast)
import Data.DList (singleton)
import Control.Monad.State

run :: MonadIO m => ArgData -> Program -> m (Bool, VState, Log)
run args prog = do
--   let wlps = makeWLPs (LitB True) (progToExecMaxDepth (maxLength args) prog)
--   (res, st, logs) <- liftIO $ runV (ReaderData args) (testAllPaths wlps)
--   return (res, st, logs)
  -- liftIO $ print (singleton $ show (progToExecMaxDepth (maxLength args) prog))
  liftIO $ runV (ReaderData args) (prunedCalcWLP (pruneInfeasible (enabledHeuristics args)) $ progToExecMaxDepth (maxLength args) prog)

-- (pruneInfeasible (enabledHeuristics args))

testAllPaths :: (MonadZ3 m, MonadG m) => [Expr] -> m Bool
testAllPaths []     = tell (singleton "Accept\n") >> return True
testAllPaths (e:es) = do
  incrNumPaths
  whenRs (dumpConditions . options) $
    tell $ singleton (show e)
  res <- getValidityCounterExample [] =<< expr2ast e
  case res of
    -- No counterexample found, check next WLP.
    Nothing -> testAllPaths es
     -- Counterexample found, stop here.
    Just ex -> tell (singleton $ unlines ["Reject\n", "Variable assignments:", ex]) >> return False