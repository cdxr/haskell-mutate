{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Control.Mutate
Copyright   : (c) Craig Roche 2013
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable

This module provides abstractions for working with variables that are mutable
over a particular base monad. It defines the typeclasses 'WriteVar', 'EditVar',
and 'ReadVar', representing values that may be replaced, modified, and retrieved.

It also provides two abstract types:

* 'Edit' m s - \"Modify-only\" variables isomorphic to @(s -> s) -> m ()@

* 'Write' m s - \"Replace-only\" variables isomorphic to @s -> m ()@
-}

module Control.Mutate (
 -- * Types
   Edit
 , edit
 , Write
 , write
 -- * Reading
 , ReadVar ( readVar )
 , askVar
 -- * Writing
 , WriteVar ( writeVar )
 , putVar
 -- * Editing
 , EditVar ( editVar, editVar' )
 , modifyVar
 , modifyVar'
 -- * Utilities
 , tryModifyMVar
 , tryModifyTMVar
) where


import Control.Monad
import Control.Monad.Reader.Class

import Data.IORef
import Control.Concurrent.MVar

import Control.Concurrent.STM ( STM )
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad.Base


-- | @Edit m s@ represents a state @s@ that is mutable in a monad @m@.
-- There is no way to observe the internal state in general.
newtype Edit m s = Edit { runEdit :: (s -> s) -> m () }

-- | Encapsulate an @EditVar@ in an @Edit@.
edit :: (EditVar m v) => v s -> Edit m s
edit = Edit . editVar


-- | @Write m s@ represents a shared state @s@ that can be written to in a
-- monad @m@.
newtype Write m s = Write { runWrite :: s -> m () }

-- | Encapsulate a @WriteVar@ in a @Write@.
write :: (WriteVar m v) => v s -> Write m s
write = Write . writeVar


-- | This class represents observable state.
-- 'readVar' must not modify any values, and must not block.
--
--
-- @
-- 'readVar' v >> 'return' a === 'return' a
-- @
--
class ReadVar m v | v -> m where
    readVar :: v s -> m s

instance ReadVar IO IORef where
    readVar = readIORef

instance ReadVar STM TVar where
    readVar = readTVar

{-
-- This instance would require UndecidableInstances

instance (MonadIO m, MonadPlus m) => ReadVar m MVar where
    readVar = maybe mzero return =<< tryTakeMVar
-}

--instance ReadVar Identity where
--    readVar   = return . runIdentity

{-
-- This could be useful, but it does not guarantee the ReadVar properties:

instance ReadVar (STM s) s where
    readVar = id

-- The purpose of ReadVar is to offer tighter semantic constraints than general
-- STM.
-}


-- | This class represents a shared value that may be replaced.
-- 'writeVar' must not block.
--
-- @
-- 'writeVar' v s === 'writeVar' v a >> 'writeVar' v s
-- @
--
-- If a type is also an instance of 'ReadVar', the following
-- must hold:
--
-- @
-- 'readVar' v >>= 'writeVar' v === return ()
-- 'writeVar' v a >> 'readVar' v === 'writeVar' v a >> return a
-- @
--
class WriteVar m v | v -> m where
    writeVar :: v s -> s -> m ()

instance WriteVar IO IORef where
    writeVar = writeIORef

instance WriteVar STM TVar where
    writeVar = writeTVar

instance WriteVar STM TMVar where
    writeVar v = void . tryPutTMVar v

instance WriteVar m (Edit m) where
    writeVar v = editVar v . const

instance WriteVar m (Write m) where
    writeVar = runWrite


-- | This class represents modifiable shared state.
-- 'editVar' must not block. However, it may still be defined for types with 
-- potentially absent values, e.g. 'TMVar'.
--
-- @
-- 'writeVar' v a === 'editVar' v (const a)
-- 'editVar' v f >> 'editVar' v g === 'editVar' v (g . f)
-- 'editVar'' v f = 'editVar' v $! f
-- @
--
-- Minimal complete definition: 'editVar'
  
class (WriteVar m v) => EditVar m v | v -> m where
    editVar  :: v s -> (s -> s) -> m ()

    -- | A strict version of 'editVar'
    editVar' :: v s -> (s -> s) -> m ()
    editVar' v f = editVar v $! f

instance EditVar IO IORef where
    editVar  = modifyIORef
    editVar' = modifyIORef'

--instance EditVar IO MVar where
--    editVar v = void . tryModifyMVar v

instance EditVar STM TVar where
    editVar  = modifyTVar
    editVar' = modifyTVar'
 
instance EditVar STM TMVar where
    -- maps the function over the stored value, if it exists
    editVar v = void . tryModifyTMVar v

instance EditVar m (Edit m) where
    editVar = runEdit


-- | Read the value of the variable in a monadic context.
--
-- For the @(->)@ instance of @MonadReader@, 'askVar' is equivalent
-- to 'readVar'.
askVar :: (MonadBase b m, ReadVar b v, MonadReader (v s) m) => m s
askVar = liftBase . readVar =<< ask

-- | Write to the variable in a monadic context.
--
-- For the @(->)@ instance of @MonadReader@, 'putVar' is equivalent
-- to @flip 'writeVar'@.
putVar :: (MonadBase b m, WriteVar b v) => s -> v s -> m ()
putVar s v = liftBase $ writeVar v s

modifyVar :: (MonadBase b m, EditVar b v) => (s -> s) -> v s -> m ()
modifyVar f v = liftBase $ editVar v f

-- | A strict version of 'modifyVar'
modifyVar' :: (MonadBase b m, EditVar b v) => (s -> s) -> v s -> m ()
modifyVar' f v = liftBase $ editVar' v f



-- | Modify the value of an 'MVar' if it is non-empty. Returns True if
-- the modification was applied, or False if the 'MVar' is empty.
tryModifyMVar :: MVar a -> (a -> a) -> IO Bool
tryModifyMVar v f = do
    mx <- tryTakeMVar v
    case mx of
        Just x -> tryPutMVar v (f x)
        Nothing -> return False

-- | Modify the value of a 'TMVar' if it is non-empty. Returns True if
-- the modification was applied, or False if the 'TMVar' is empty.
tryModifyTMVar :: TMVar a -> (a -> a) -> STM Bool
tryModifyTMVar v f = do
    mx <- tryTakeTMVar v
    case mx of
        Just x -> tryPutTMVar v (f x)
        Nothing -> return False
