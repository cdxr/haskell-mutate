{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Control.Concurrent.Var
Copyright   : (c) Craig Roche 2013
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable

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

-- | Encapsulate any @EditVar v s@ in an @Edit s@.
edit :: (EditVar v s) => v -> Edit s
edit = Edit . editVar


-- | An abstract type representing a shared state that can be replaced.
newtype Write s = Write { runWrite :: s -> STM () }

-- | Encapsulate any @WriteVar v s@ in a @Write s@.
write :: (WriteVar v s) => v -> Write s
write = Write . writeVar


-- | This class represents shared observable state.
-- 'readVar' must not modify any values, and must not block.
--
-- @
-- 'readVar' v >> 'return' a === 'return' a
-- 'readVarIO' = 'atomically' . 'readVar'
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
-- This could be useful, but it does not guarantee the ReadVar properties:

instance ReadVar (STM s) s where
    readVar = id
    readVarIO = atomically

-- The purpose of ReadVar is to offer tighter semantic constraints than general
-- STM.
-}


-- | This class represents modifiable shared state.
-- 'editVar' must not block. However, it may still be defined for types with 
-- potentially absent values, e.g. 'TMVar'.
--
-- @
-- 'editVar' v f >> 'editVar' v g === 'editVar' v (g . f)
-- 'editVar'' v f = 'editVar' v $! f
-- @
--
-- If a type is also an instance of 'ReadVar', the following
-- must hold:
--
-- @
-- do 'editVar' v f   ===   do s <- 'readVar' v
--    'readVar' v              'editVar' v f
--                           return (f s)
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


-- | This class represents a shared value that may be replaced.
-- 'writeVar' must not block.
--
-- @
-- 'writeVar' v s === 'writeVar' v a >> 'writeVar' v s
-- @
--
-- If a type is also an instance of 'EditVar', the following must hold:
--
-- @
-- 'writeVar' v a === 'editVar' v (const a)
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


-- | Read the value of the variable in a monadic context.
--
-- For the @((->) v)@ instance of @(MonadReader v m)@, 'askVar' is equivalent
-- to 'readVar'.
askVar :: (ReadVar v s, MonadReader v m) => m (STM s)
askVar = asking readVar

askVarIO :: (ReadVar v s, MonadReader v m) => m (IO s)
askVarIO = asking readVarIO

-- | Write to the variable in a monadic context.
--
-- For the @((->) v)@ instance of @(MonadReader v m)@, 'putVar' is equivalent
-- to @flip 'writeVar'@.
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


-- | Modify the value of the 'TMVar' if it is non-empty. Returns True if
-- the modification was applied, or False if the 'TMVar' is empty.
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
