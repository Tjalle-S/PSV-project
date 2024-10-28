{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Util (
  VState(..), Stats(..), ReaderData(..), Log

, V, runV
, GT, MonadG

, incrNumPaths, incrFormulaSize, incrNumPruned

, isEnabled, whenRs, optionalError) where

import Cli (ArgData (enableAllHeuristics, enabledHeuristics), HeuristicOptions)
import Z3.Monad (Z3, evalZ3)
import Control.Monad.RWS (RWST (runRWST), MonadRWS, MonadState, MonadReader, asks, modify)
import Data.DList (DList)
import Control.Monad (when)

-- TODO: find better names.

data ReaderData = ReaderData {
  options :: ArgData
-- , vars :: Map String Symbol
}

data VState = VState {
  stats :: Stats
} deriving Show

-- | Stats that need to kept track of during the process.
data Stats = Stats {
  -- | Total number of inspected paths.
  inspectedPaths  :: Int
  -- | Total number of paths pruned due to being infeasible.
, infeasiblePaths :: Int
  -- | Total size of formulae to verify.
, formulaSize     :: Int
-- Note that total computation time does not need to be kept as state.
} deriving Show

type Log = DList String

type GT = RWST ReaderData Log VState

type MonadG = MonadRWS ReaderData Log VState

-- | Monad type for computations using Z3.
type V = GT Z3

-- | Execute a computation in the verifier's monad.
runV :: ReaderData -> V a -> IO (a, VState, Log)
runV args mx = evalZ3 $ runRWST mx args emptyState

emptyState :: VState
emptyState = VState {
  stats = Stats {
    inspectedPaths  = 0
  , infeasiblePaths = 0
  , formulaSize     = 0
  }
}

-- | An error message to show that a bonus feature is not yet implemented.
optionalError :: a
optionalError = error "Not implemented: optional assignment"

incrNumPaths, incrFormulaSize, incrNumPruned :: MonadState VState m => m ()
incrNumPaths    = modify $ \v@VState { stats = s@Stats { inspectedPaths } }  ->
  v { stats = s { inspectedPaths  = inspectedPaths  + 1 } }
incrFormulaSize = modify $ \v@VState { stats = s@Stats { formulaSize } }     ->
  v { stats = s { formulaSize     = formulaSize     + 1 } }
incrNumPruned   = modify $ \v@VState { stats = s@Stats { infeasiblePaths } } ->
  v { stats = s { infeasiblePaths = infeasiblePaths + 1 } }

isEnabled :: (HeuristicOptions -> Bool) -> ReaderData -> Bool
isEnabled f = (||) <$> enableAllHeuristics <*> f . enabledHeuristics <$> options

-- | A variant of 'Control.Monad.when' where the condition is read from an environment.
whenRs :: (MonadReader env m) => (env -> Bool) -> m () -> m ()
whenRs getC a = do
  c <- asks getC
  when c a
