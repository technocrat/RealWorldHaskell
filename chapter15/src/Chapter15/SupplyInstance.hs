{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Chapter15.SupplyInstance where

import           Chapter15.SupplyClass

import           Control.Monad         (liftM)

newtype Reader e a = R { runReader :: e -> a }

instance Monad (Reader e) where
    -- return a = R $ \_ -> a
    return = R . const
    m >>= k = R $ \r -> runReader (k (runReader m r)) r

ask :: Reader e e
ask = R id

newtype MySupply e a = MySupply { runMySupply :: Reader e a }
                       deriving Monad

instance MonadSupply e (MySupply e) where
    -- next = MySupply $ do
    --          v <- ask
    --          return $ Just v
    next = MySupply $ Just `liftM` ask

xy :: (Num s, MonadSupply s m) => m s
xy = do
  Just x <- next
  Just y <- next
  return $ x * y

runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply
