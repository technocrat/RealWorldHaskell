module Chapter18.StackStack where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State

type Foo = StateT Int (State String)

outerPut :: Int -> Foo ()
outerPut = put

innerPut :: String -> Foo ()
innerPut = lift . put

type Bar = ReaderT Bool Foo

barPut :: String -> Bar ()
barPut = lift . lift . put
