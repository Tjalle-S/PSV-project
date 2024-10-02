-- This module makes monad transformers an instance of the MonadZ3 type class.

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Z3Instance () where

import Z3.Monad ( MonadZ3(..) )

import Control.Monad.Trans    ( MonadTrans(..) )
import Control.Monad.IO.Class ( MonadIO        )

instance (MonadZ3 m, MonadTrans t, Applicative (t m), Monad (t m), MonadIO (t m)) => MonadZ3 (t m) where
  getSolver  = lift getSolver
  getContext = lift getContext
