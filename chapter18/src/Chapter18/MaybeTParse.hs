{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter18.MaybeTParse (Parse, evalParse) where

import           Chapter18.MaybeT
import           Control.Monad.State
import qualified Data.ByteString.Lazy as L
import           Data.Int             (Int64)

data ParseState = ParseState { string :: L.ByteString
                             , offset :: Int64
                             } deriving Show

newtype Parse a = P {
      runP :: MaybeT (State ParseState) a
    } deriving (Monad, MonadState ParseState)

evalParse :: Parse a -> L.ByteString -> Maybe a
evalParse m s = evalState (runMaybeT (runP m)) (ParseState s 0)
