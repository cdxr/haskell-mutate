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
 -- * Writing
 , WriteVar ( writeVar )
 -- * Editing
 , EditVar ( editVar, editVar' )
) where


import Data.IORef

import Control.Monad.ST.Safe
import Data.STRef

import Control.Concurrent.STM ( STM )
import Control.Concurrent.STM.TVar


-- | @Edit m s@ represents a state @s@ that is mutable in a monad @m@.
-- @Edit@ does not provide a means to observe the internal state.
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
-- @
-- 'readVar' v >> 'return' a === 'return' a
-- @
--
class ReadVar m v | v -> m where
    readVar :: v s -> m s

instance ReadVar IO IORef where
    readVar = readIORef

instance ReadVar (ST s) (STRef s) where
    readVar = readSTRef

instance ReadVar STM TVar where
    readVar = readTVar


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

instance WriteVar (ST s) (STRef s) where
    writeVar = writeSTRef

instance WriteVar STM TVar where
    writeVar = writeTVar

instance WriteVar m (Edit m) where
    writeVar v = editVar v . const

instance WriteVar m (Write m) where
    writeVar = runWrite


-- | This class represents mutable state that can be mapped over.
-- 'editVar' must not block.
--
-- @
-- 'writeVar' v a === 'editVar' v (const a)
-- 'editVar' v f >> 'editVar' v g === 'editVar' v (g . f)
-- 'editVar'' v f = 'editVar' v $! f
-- @
--
-- Minimal complete definition: 'editVar'
--
class (WriteVar m v) => EditVar m v | v -> m where
    editVar  :: v s -> (s -> s) -> m ()

    -- | A strict version of 'editVar'
    editVar' :: v s -> (s -> s) -> m ()
    editVar' v f = editVar v $! f

instance EditVar IO IORef where
    editVar  = modifyIORef
    editVar' = modifyIORef'

instance EditVar (ST s) (STRef s) where
    editVar  = modifySTRef
    editVar' = modifySTRef'

instance EditVar STM TVar where
    editVar  = modifyTVar
    editVar' = modifyTVar'
 
instance EditVar m (Edit m) where
    editVar = runEdit
