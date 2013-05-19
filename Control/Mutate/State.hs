{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Mutate.State
(
  VarState
, varState 
, (%:) 
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


varState :: (MonadBase b m, ReadVar b v, WriteVar b v)
         => VarState s m a -> v s -> m a
varState (VS f) = f . mkVar


(%:) :: (MonadBase b m, ReadVar b v, WriteVar b v)
     => v s -> VarState s m a -> m a
(%:) = flip varState
