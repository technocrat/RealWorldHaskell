module Chapter13.Monoid where

import           Chapter13.DList (DList, append, empty)

import           Data.Monoid

instance Monoid (DList a) where
    mempty = empty
    mappend = append
