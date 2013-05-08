{-# LANGUAGE FlexibleContexts #-}

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

* 'Edit'  - "Modify-only" variable isomorphic to @(s -> s) -> STM ()@

* 'Write' - "Replace-only" variable isomorphic to @s -> STM ()@
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
 -- * Writing
 , WriteVar ( writeVar )
 , putVar
 -- * Editing
 , EditVar ( editVar , editVar' )
 , modifyVar
 , modifyVar'
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

-- | Encapsulate an editable variable in an @Edit v@ so that it cannot be observed.
edit :: (EditVar v) => v s -> Edit s
edit = Edit . editVar


-- | An abstract type representing a shared state that can be replaced.
newtype Write s = Write { runWrite :: s -> STM () }

-- | Encapsulate a value in a @Write v@ so that it can only be replaced.
write :: (WriteVar v) => v s -> Write s
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
class ReadVar v where
    readVar   :: v s -> STM s

    readVarIO :: v s -> IO s
    readVarIO = atomically . readVar

instance ReadVar TVar where
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
class EditVar v where
    editVar  :: v s -> (s -> s) -> STM ()

    editVar' :: v s -> (s -> s) -> STM ()
    editVar' v f = editVar v $! f

instance EditVar TVar where
    editVar  = modifyTVar
    editVar' = modifyTVar'
 
instance EditVar TMVar where
    -- maps the function over the stored value, if it exists
    editVar v = void . tryModifyTMVar v

instance EditVar Edit where
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
class WriteVar v where
    writeVar :: v s -> s -> STM ()

instance WriteVar TVar where
    writeVar = writeTVar

instance WriteVar TMVar where
    writeVar v = void . tryPutTMVar v

instance WriteVar Edit where
    writeVar v = editVar v . const

instance WriteVar Write where
    writeVar = runWrite


askVar :: (ReadVar v, MonadReader (v s) m) => m (STM s)
askVar = asking readVar

askVarIO :: (ReadVar v, MonadReader (v s) m) => m (IO s)
askVarIO = asking readVarIO

putVar :: (WriteVar v, MonadReader (v s) m) => s -> m (STM ())
putVar = asking . flip writeVar

modifyVar :: (EditVar v, MonadReader (v s) m) => (s -> s) -> m (STM ())
modifyVar = asking . flip editVar

-- | A strict version of 'modifyVar'
modifyVar' :: (EditVar v, MonadReader (v s) m) => (s -> s) -> m (STM ())
modifyVar' = asking . flip editVar'


-- | Joins the 'STM' output from a 'MonadSTM' transformer stack with the
-- stack itself.
--
-- example:
--
-- @
-- f :: Int -> ReaderT (Edit Int) STM (STM Int)
-- f x = fmap (+x) getVar
--
-- joinSTM . f :: Int -> ReaderT (Edit Int) STM Int
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
