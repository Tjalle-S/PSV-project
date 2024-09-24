{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Util (VState(..), Stats(..), V, runV, GT, MonadG, ReaderData(..)) where

import Cli (ArgData)
import Z3.Monad (Z3, evalZ3, Symbol)
import Control.Monad.RWS (RWST (runRWST), MonadRWS, MonadWriter (tell), modify, gets, ask)
-- import Data.Map (Map)

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

type Log = [String]

type GT = RWST ReaderData Log VState

type MonadG = MonadRWS ReaderData Log VState

test :: MonadG m => a -> m a
test x = do
  y <- ask
  tell ["hello log!"]
  modify modState
  z <- gets (inspectedPaths . stats)
  undefined

modState s = s { stats = (stats s) { inspectedPaths = 0 } }

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
