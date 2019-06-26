{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.ForgetT where

import Control.Applicative

import Control.Monad.Writer.Lazy

-- base monad classes
import Control.Monad.Fail (MonadFail)
-- mtl monad classes
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Cont.Class (MonadCont)

newtype ForgetT w m a = ForgetT {unForgetT :: WriterT (Any, w) m a}
  deriving (
    Foldable, Traversable,
    Functor, Applicative, Monad, MonadTrans,
    Alternative, MonadPlus,
    MonadFail, MonadIO, MonadFix,
    MonadError , MonadReader, MonadState, MonadCont,
    Eq, Ord, Read, Show)
    
deriving instance (Monoid w, MonadError e m) => MonadError e (ForgetT w m)
deriving instance (Monoid w, MonadReader r m) => MonadReader r (ForgetT w m)
deriving instance (Monoid w, MonadState s m) => MonadState s (ForgetT w m)
 
-- Eq1, Ord1, Read1, Show1 ,
 
{-  
instance (Monoid w, Applicative m, Semigroup a) => Semigroup (ForgetT w m a)
  where
    (<>) = liftA2 (<>)
-}
    
instance (Monoid w, Applicative m, Monoid a) => Monoid (ForgetT w m a)
  where
    mempty = pure mempty
    mappend = liftA2 mappend

instance (Monoid w, Monad m) => MonadWriter w (ForgetT w m) where
    writer = ForgetT . writer . fmap (Any False,)
    tell = ForgetT . tell . (Any False,)
    listen = ForgetT . fmap (fmap snd) . listen . unForgetT
    pass = ForgetT . pass . fmap (fmap fmap) . unForgetT
