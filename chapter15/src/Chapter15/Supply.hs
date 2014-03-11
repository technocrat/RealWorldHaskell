{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter15.Supply
    ( Supply
    , next
    , runSupply
    ) where

import Control.Monad.State

newtype Supply s a = S (State [s] a)
                     deriving Monad

{-
unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

instance Monad (Supply s) where
    s >>= m = S (unwrapS >>= unwrapS . m)
    return = S . return
-}

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) = runState m

next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of
                []     -> return Nothing
                (x:xs) -> do put xs
                             return $ Just x
