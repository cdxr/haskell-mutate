{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Module      : Control.Mutate.Atomic
Copyright   : (c) 2013 Craig Roche
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable

-}

module Control.Mutate.Atomic where

import Control.Monad
import Control.Monad.Trans.State
import Control.Concurrent.MVar

import Data.StateVar ( StateVar )  -- used in haddock documentation
import Data.IORef    ( IORef )     -- used in haddock documentation

import Data.Tuple ( swap )

import Control.Mutate.Mono


-- * AtomicVar

-- | @Atomic m v@ indicates that the contents of the variable @v@ may be
-- modified, replaced, or retrieved atomically in the monad @m@.
--
-- Minimal complete definition: 'editVar'
--
class (Monad m) => AtomicVar m v | v -> m where
    -- | @editAtomic v f@ is a computation that uses the computation @f@ to
    -- atomically modify the contents of @v@ and output a value.
    --
    -- This will block if the variable @v@ is locked in another thread.
    editAtomic :: v s -> (s -> m (a, s)) -> m a

    -- | A version of `editAtomic` that takes a computation with no output.
    editAtomic_ :: v s -> (s -> m s) -> m ()
    editAtomic_ v f = editAtomic v (liftM ((,) ()) . f)


readAtomic :: (AtomicVar m v) => v s -> m s
readAtomic v = editAtomic v $ \s -> return (s, s)

writeAtomic :: (AtomicVar m v) => v s -> s -> m ()
writeAtomic v s = editAtomic_ v $ \_ -> return s

stateAtomic :: (AtomicVar m v) => v s -> StateT s m a -> m a
stateAtomic v = editAtomic v . runStateT

stateAtomic_ :: (AtomicVar m v) => v s -> StateT s m a -> m ()
stateAtomic_ v = editAtomic_ v . execStateT


instance AtomicVar IO MVar where
    editAtomic v f = modifyMVar v (liftM swap . f)
    editAtomic_ = modifyMVar_


-- * Atomic

newtype Atomic m s = Atomic { runAtomic :: forall a. (s -> m (a, s)) -> m a }


instance (Monad m) => AtomicVar m (Atomic m) where
    editAtomic = runAtomic


-- | Create a new @Atomic IO s@. This is an `MVar` that is restricted to
-- using only atomic operations (every `takeMVar` is paired with a `putMVar`).
newAtomicIO :: s -> IO (Atomic IO s)
newAtomicIO s = do
    v <- newMVar s
    return $ Atomic $ editAtomic v


-- | Create a locked resource from a readable and writable variable. The
-- primary use of this function is to convert a `StateVar` into
-- a concurrent resource.
--
-- This can also be used to convert an `IORef` to an `Atomic`, but in that
-- case it is better to use an `MVar` or to create one with `newAtomicIO`.
--
-- WARNING: access to the resource is only atomic if all clients of the
-- variable access it through this returned `Atomic` value.
--
atomicLock :: (ReadVar IO v, WriteVar IO v) => v s -> IO (Atomic IO s)
atomicLock v = do
    lock <- newMVar ()
    return $ Atomic $ \f ->
        withMVar lock $ \_ -> do
            (a, s') <- f =<< readVar v
            writeVar v s'
            return a
