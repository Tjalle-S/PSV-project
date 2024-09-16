module Util where

import Control.Monad.Reader (ReaderT)
import Cli (ArgData)
import Control.Monad.State (StateT)
import Z3.Monad (Z3)

-- TODO: find better names.

data VState = VState {
  stats :: Stats
}

-- | Stats that need to kept track of during the process.
data Stats = Stats {
  -- | Total number of inspected paths.
  inspectedPaths  :: Int
  -- | Total number of inspected paths found infeasible.
, infeasiblePaths :: Int
  -- | Total size of formulae to verify.
, formulaSize     :: Int
-- Note that total computation time does not need to be kept as state.
}

-- | Monad transformer type for computations.
type V = ReaderT ArgData (StateT VState Z3)