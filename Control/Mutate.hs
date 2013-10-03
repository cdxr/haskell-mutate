{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Control.Mutate
Copyright   : (c) 2013 Craig Roche
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable

This module provides abstractions for working with variables that are mutable
in a particular base monad. It defines the typeclasses 'WriteVar', 'EditVar',
and 'ReadVar', representing values that may be replaced, modified, and retrieved.

It also provides two abstract types:

* 'Edit' m s - \"Modify-only\" variables isomorphic to @(s -> s) -> m ()@

* 'Write' m s - \"Replace-only\" variables isomorphic to @s -> m ()@

NOTE: This module deals only with mutable state, not concurrency.
Instances defined in IO are not thread-safe, and their laws are only valid
provided that the variables are not accessed by multiple threads. For shared
mutable state, see Control.Mutate.Atomic.
-}

module Control.Mutate (
    -- * Typeclasses
    -- ** ReadVar
    ReadVar ( readVar ),
    -- ** WriteVar
    WriteVar ( writeVar ),
    -- ** EditVar
    EditVar ( editVar, editVar' ),

    -- * Abstract Types
    -- ** Write
    Write,
    write,
    mapWrite,
    -- ** Edit
    Edit,
    edit,
    mapEdit,
    ) where


import Data.IORef

import Data.StateVar as StateVar

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
{-# INLINABLE edit #-}

-- | @mapEdit f v@ is an `Edit` that maps @f@ over every function passed
-- to @editVar v@:
--
-- @
-- editVar (mapEdit f v) g === editVar v (f g)
-- @
--
-- @mapEdit f v@ is only a valid @Edit@ when it satisfies @f id = id@ and when
-- @v@ is a valid @Edit@.
--
mapEdit :: ((t -> t) -> s -> s) -> Edit m s -> Edit m t
mapEdit f v = Edit (runEdit v . f)
{-# INLINABLE mapEdit #-}


-- | @Write m s@ represents a state @s@ that can be overwritten in a monad @m@.
newtype Write m s = Write { runWrite :: s -> m () }

-- | Encapsulate a @WriteVar@ in a @Write@.
write :: (WriteVar m v) => v s -> Write m s
write = Write . writeVar
{-# INLINABLE write #-}

-- | @mapWrite f v@ is a `Write` that applies @f@ to every value passed to
-- @writeVar v@:
--
-- @
-- writeVar (mapWrite f v) a === writeVar v (f a)
-- @
--
mapWrite :: (t -> s) -> Write m s -> Write m t
mapWrite f v = Write (runWrite v . f)
{-# INLINABLE mapWrite #-}


-- | @ReadVar m v@ indicates that the contents of the variable @v@ may be
-- observed in the monad @m@.
-- 'readVar' must be an idempotent operation, and must not block.
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

instance ReadVar IO StateVar where
    readVar = get

instance ReadVar IO GettableStateVar where
    readVar = get


-- | @WriteVar m v@ represents that the contents of a variable @v@ can be
-- replaced in the monad @m@. 'writeVar' must not block.
--
-- @
-- 'writeVar' v s === 'writeVar' v a >> 'writeVar' v s
-- @
--
-- If @v@ is also an instance of 'ReadVar', the following
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

instance WriteVar IO SettableStateVar where
    writeVar = ($=)

instance WriteVar IO StateVar where
    writeVar = ($=)

instance WriteVar m (Edit m) where
    writeVar v = editVar v . const

instance WriteVar m (Write m) where
    writeVar = runWrite


-- | @EditVar m v@ represents that the contents of the variable @v@ can be
-- mapped over in the monad @m@. 'editVar' must not block.
--
-- @
-- 'writeVar' v a === 'editVar' v (const a)
-- 'editVar' v f >> 'editVar' v g === 'editVar' v (g . f)
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

instance EditVar IO StateVar where
    editVar  = ($~)
    editVar' = ($~!)
 
instance EditVar m (Edit m) where
    editVar = runEdit
