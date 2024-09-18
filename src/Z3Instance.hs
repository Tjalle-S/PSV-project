-- This module makes monad transformers an instance of the MonadZ3 type class.

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TupleSections #-}

module Z3Instance () where

import Z3.Monad ( MonadZ3(..) )

import Control.Monad.State  ( StateT (..) )
import Control.Monad.Writer ( WriterT(..) )
import Control.Monad.RWS    ( RWST   (..) )

instance MonadZ3 m => MonadZ3 (StateT s m) where
  getSolver  = StateT $ \s -> (, s) <$> getSolver
  getContext = StateT $ \s -> (, s) <$> getContext

instance (MonadZ3 m, Monoid w) => MonadZ3 (WriterT w m) where
  getSolver  = WriterT $ (, mempty) <$> getSolver
  getContext = WriterT $ (, mempty) <$> getContext

instance (MonadZ3 m, Monoid w) => MonadZ3 (RWST r w s m) where
  getSolver  = RWST $ \_ s -> (, s, mempty) <$> getSolver
  getContext = RWST $ \_ s -> (, s, mempty) <$> getContext