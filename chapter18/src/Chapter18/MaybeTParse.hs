{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter18.MaybeTParse  where

import           Chapter18.MaybeT

import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.ByteString.Lazy      (ByteString)
import           Data.Int                  (Int64)

data ParseState = ParseState { string :: ByteString
                             , offset :: Int64
                             } deriving Show

newtype Parse a = P { runP :: MaybeT (State ParseState) a }
                deriving (Functor, Applicative, Monad)

evalParse :: Parse a -> ByteString -> Maybe a
evalParse m s = evalState ((runMaybeT . runP) m) (ParseState s 0)
