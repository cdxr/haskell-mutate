{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Control.Mutate.State
Copyright   : (c) Craig Roche 2013
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable

This module provides the abstract monad transformer 'VarState'.
@VarState s m@ represents a state value of type @s@ that can be modified in
a monad @m@.
-}

module Control.Mutate.State
(
  VarState
, runVarState 
, askVarState
, (%:) 
, module Control.Monad.State.Class
)
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Base

import Control.Mutate


data Var m s = Var (m s) (s -> m ())

instance ReadVar m (Var m) where
    readVar (Var r _) = r

instance WriteVar m (Var m) where
    writeVar (Var _ w) = w

instance (Monad m) => EditVar m (Var m) where
    editVar (Var r w) f = r >>= w . f

mkVar :: (MonadBase b m, ReadVar b v, WriteVar b v) => v s -> Var m s
mkVar v = Var (liftBase $ readVar v) (liftBase . writeVar v)


newtype VarState s m a = VS { runVar :: Var m s -> m a }

mapVar :: (m a -> m b) -> VarState s m a -> VarState s m b
mapVar f (VS m) = VS $ f . m

liftVar :: m a -> VarState s m a
liftVar = VS . const

instance (Functor m) => Functor (VarState s m) where
    fmap f = mapVar $ fmap f

instance (Applicative m) => Applicative (VarState s m) where
    pure = liftVar . pure
    VS f <*> VS g = VS $ \x -> f x <*> g x

instance (Monad m) => Monad (VarState s m) where
    return = liftVar . return
    VS m >>= f = VS $ \vse -> do
        a <- m vse
        runVar (f a) vse

instance MonadTrans (VarState s) where
    lift = liftVar

instance (Monad m) => (MonadState s (VarState s m)) where
    get = VS readVar
    put = VS . flip writeVar


runVarState :: (MonadBase b m, ReadVar b v, WriteVar b v)
            => VarState s m a -> v s -> m a
runVarState (VS f) = f . mkVar

askVarState :: (MonadBase b m, ReadVar b v, WriteVar b v, MonadReader (v s) m)
            => VarState s m a -> m a
askVarState m = runVarState m =<< ask

-- | An infix version of runVarState. This facilitates the use of
-- MonadState computations to modify any instance of ReadVar and WriteVar.
--
-- Example:
--
-- > var <- newIORef (2,3)
-- > var %: do
-- >    a <- gets fst
-- >    put (5,8)
-- >    modify $ \(x,y) -> (x,a)
-- > print =<< readIORef var  -- prints (5,2)
-- 
(%:) :: (MonadBase b m, ReadVar b v, WriteVar b v)
     => v s -> VarState s m a -> m a
(%:) = flip runVarState
