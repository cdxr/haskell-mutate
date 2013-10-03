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

import Data.Tuple ( swap )


-- | @Atomic m v@ indicates that the contents of the variable @v@ may be
-- modified, replaced, or retrieved atomically in the monad @m@.
-- Any use of the variable can block because another thread could be performing
-- an atomic operation.
--
class (Monad m) => AtomicVar m v | v -> m where
    editAtomic :: v s -> (s -> m (a, s)) -> m a

    editAtomic_ :: v s -> (s -> m s) -> m ()
    editAtomic_ v f = editAtomic v (liftM ((,) ()) . f)


readAtomic :: (AtomicVar m v) => v s -> m s
readAtomic v = editAtomic v $ \s -> return (s, s)


stateAtomic :: (AtomicVar m v) => v s -> StateT s m a -> m a
stateAtomic v = editAtomic v . runStateT

stateAtomic_ :: (AtomicVar m v) => v s -> StateT s m a -> m ()
stateAtomic_ v = editAtomic_ v . execStateT


instance AtomicVar IO MVar where
    editAtomic v f = modifyMVar v (liftM swap . f)
    editAtomic_ = modifyMVar_
