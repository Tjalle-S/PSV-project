{-# LANGUAGE FlexibleContexts #-}

module Runner (run) where

import GCLParser.GCLDatatype (Program)
import Control.Monad.Writer (MonadWriter (tell), MonadIO (liftIO))
import Cli (ArgData (..))
import Expr (Expr(LitB))
import WLP (makeWLPs)
import TreeBuilder (progToExecMaxDepth)
import Util (runV, ReaderData (..), VState, incrNumPaths)
import Z3.Monad (MonadZ3)
import Control.Monad.State (MonadState)
import Z3Instance ()
import Z3Util (getValidityCounterExample, expr2ast)

run :: (MonadWriter String m, MonadIO m) => ArgData -> Program -> m Bool
run args prog = do
  let wlps = makeWLPs (LitB True) (progToExecMaxDepth (maxLength args) prog)
  (res, _, _) <- liftIO $ runV (ReaderData args) (testAllPaths wlps)
  return res

testAllPaths :: (MonadZ3 m, MonadState VState m, MonadWriter String m) => [Expr] -> m Bool
testAllPaths []     = tell "Accept\n\n" >> return True
testAllPaths (e:es) = do
  incrNumPaths
  res <- getValidityCounterExample =<< expr2ast e
  case res of
    Nothing -> testAllPaths es                                                          -- No counterexample found, check next WLP.
    Just ex -> tell (unlines ["Reject\n", "Variable assignments:", ex]) >> return False -- Counterexample found, stop here.
