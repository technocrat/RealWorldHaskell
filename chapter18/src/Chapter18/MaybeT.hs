module Chapter18.MaybeT where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

bindMT :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do unwrapped <- runMaybeT x
                           case unwrapped of Nothing -> return Nothing
                                             Just y  -> runMaybeT (f y)

altBindMT :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `altBindMT` f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

returnMT :: Monad m => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: Monad m => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance Monad m => Monad (MaybeT m) where
    return = returnMT
    (>>=) = bindMT
    fail = failMT

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

instance Functor m => Functor (MaybeT m) where
    fmap f = mapMaybeT (fmap (fmap f))

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

instance MonadIO m => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

{-
instance MonadState s m => MonadState s (MaybeT m) where
    get = lift get
    put k = lift (put k)
-}
