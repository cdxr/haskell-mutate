{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Control.Concurrent.Var
Copyright   : (c) Craig Roche 2013
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable (STM and FlexibleContexts)

This module provides combinators for working with shared mutable state. It
defines the typeclasses 'WriteVar', 'EditVar', and 'ReadVar', for
working with values that may be replaced, modified, and retrieved.

It also provides two abstract types:

* 'Edit'  - \"Modify-only\" variable isomorphic to @(s -> s) -> STM ()@

* 'Write' - \"Replace-only\" variable isomorphic to @s -> STM ()@
-}

module Control.Concurrent.Var (
 -- * Types
   Edit
 , edit
 , Write
 , write
 -- * Reading
 , ReadVar ( readVar, readVarIO )
 , askVar
 , askVarIO
 -- * Editing
 , EditVar ( editVar , editVar' )
 , modifyVar
 , modifyVar'
 -- * Writing
 , WriteVar ( writeVar )
 , putVar
 -- * Utilities
 , joinSTM
 , tryModifyTMVar
) where


import Control.Monad
import Control.Monad.Reader.Class

import Control.Concurrent.STM ( STM, atomically )
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar ( TMVar, tryTakeTMVar, tryPutTMVar )

import Control.Monad.STM.Class


-- | An abstract type representing a shared state that can be modified.
-- There is no way to observe the internal state.
newtype Edit s = Edit { runEdit :: (s -> s) -> STM () }

-- | Encapsulate an editable variable in an @Edit v@.
edit :: (EditVar v s) => v -> Edit s
edit = Edit . editVar


-- | An abstract type representing a shared state that can be replaced.
newtype Write s = Write { runWrite :: s -> STM () }

-- | Encapsulate a writable variable in a @Write v@.
write :: (WriteVar v s) => v -> Write s
write = Write . writeVar


-- | This class represents STM transactions that observe shared state.
-- 'readVar' must not modify any values, and must not block.
--
-- @
-- 'readVar' v >> 'return' a === 'return' a
-- 'readVarIO' = 'atomically' . 'readVar'
-- @
--
-- If a type is an instance of both 'ReadVar' and 'WriteVar', the following
-- must hold:
--
-- @
-- 'writeVar' v a >> 'readVar' v === 'writeVar' v a >> return a
-- @
--
-- Minimal complete definition: 'readVar'
--
class ReadVar v s | v -> s where
    readVar   :: v -> STM s

    readVarIO :: v -> IO s
    readVarIO = atomically . readVar

instance ReadVar (TVar s) s where
    readVar = readTVar
    readVarIO = readTVarIO

--instance ReadVar Identity where
--    readVar   = return . runIdentity
--    readVarIO = return . runIdentity

{-
-- This might be very useful, but it does not fulfill the non-blocking
-- guarantee:

instance ReadVar STM where
    readVar = id
    readVarIO = atomically
-}


-- | This class represents an STM transaction that modifies a shared state.
-- 'editVar' must not block, but it may do nothing when the state is not
-- present.
--
-- @
-- 'editVar' v f >> 'editVar' v g === 'editVar' v (g . f)
-- 'editVar'' v f = 'editVar' v $! f
-- @
--
-- Minimal complete definition: 'editVar'
--
class EditVar v s | v -> s where
    editVar  :: v -> (s -> s) -> STM ()

    -- | A strict version of 'editVar'
    editVar' :: v -> (s -> s) -> STM ()
    editVar' v f = editVar v $! f

instance EditVar (TVar s) s where
    editVar  = modifyTVar
    editVar' = modifyTVar'
 
instance EditVar (TMVar s) s where
    -- maps the function over the stored value, if it exists
    editVar v = void . tryModifyTMVar v

instance EditVar (Edit s) s where
    editVar = runEdit


-- | This class represents an STM transaction that stores a shared state.
-- 'writeVar' must not block.
--
-- @
-- 'writeVar' v s === 'writeVar' v a >> 'writeVar' v s
-- @
--
-- If a type is an instance of both 'EditVar' and 'WriteVar', the following
-- must hold:
--
-- @
-- 'writeVar' v a === 'editVar' v (const a)
-- @
--
class WriteVar v s where
    writeVar :: v -> s -> STM ()

instance WriteVar (TVar s) s where
    writeVar = writeTVar

instance WriteVar (TMVar s) s where
    writeVar v = void . tryPutTMVar v

instance WriteVar (Edit s) s where
    writeVar v = editVar v . const

instance WriteVar (Write s) s where
    writeVar = runWrite


askVar :: (ReadVar v s, MonadReader v m) => m (STM s)
askVar = asking readVar

askVarIO :: (ReadVar v s, MonadReader v m) => m (IO s)
askVarIO = asking readVarIO

putVar :: (WriteVar v s, MonadReader v m) => s -> m (STM ())
putVar = asking . flip writeVar

modifyVar :: (EditVar v s, MonadReader v m) => (s -> s) -> m (STM ())
modifyVar = asking . flip editVar

-- | A strict version of 'modifyVar'
modifyVar' :: (EditVar v s, MonadReader v m) => (s -> s) -> m (STM ())
modifyVar' = asking . flip editVar'


-- | Join the 'STM' output of a monad transformer stack with the stack itself.
-- Note that due to the 'MonadSTM' context this will work with a base monad
-- of either 'STM' or 'IO'.
--
-- examples:
--
-- @
-- -- Return one plus the internal Int
-- readPlusOne :: ReaderT (TVar Int) STM Int
-- readPlusOne = fmap (+1) (joinSTM askVar)
-- @
--
-- @
-- -- Add to the internal Int
-- addN :: Int -> ReaderT (Edit Int) IO ()
-- addN n = joinSTM $ modifyVar (+ n)
-- @
--
joinSTM :: (MonadSTM m, Monad m) => m (STM a) -> m a
joinSTM m = liftSTM =<< m


-- | Determine if a TMVar is not empty, and if not, apply a function to its
-- value.
tryModifyTMVar :: TMVar a -> (a -> a) -> STM Bool
tryModifyTMVar v f = do
    mx <- tryTakeTMVar v
    case mx of
        Just x -> tryPutTMVar v (f x)
        Nothing -> return False

------------------------------------------------------------------------------
-- Local Helpers

asking :: (MonadReader e m) => (e -> r) -> m r
asking f = liftM f ask
