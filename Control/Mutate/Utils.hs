{-|
Module      : Control.Mutate.Utils
Copyright   : (c) 2013 Craig Roche
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable

Utilities for working with mutable variables.
-}

module Control.Mutate.Utils where

import Control.Monad.Trans.State

import Data.IORef


-- | @atomicStateIORef@ applies a `State` computation to `atomicModifyIORef`.
atomicStateIORef :: IORef s -> State s a -> IO a
atomicStateIORef ref m = atomicModifyIORef ref $ \s ->
    let (a, s') = runState m s
    in (s', a)
{-# INLINEABLE atomicStateIORef #-}


-- | Strict version of `atomicStateIORef`. This forces both the value
-- stored and the value returned.
atomicStateIORef' :: IORef s -> State s a -> IO a
atomicStateIORef' ref m = atomicModifyIORef' ref $ \s ->
    let (a, s') = runState m s
    in (s', a)
{-# INLINEABLE atomicStateIORef' #-}
