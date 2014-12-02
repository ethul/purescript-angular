module TodomvcF.TodomvcI where

import Control.Monad.Eff
import Control.Monad.Free (FreeC(), injC)
import Data.Coyoneda (Natural())
import Data.Either (either)
import Data.Foreign (F(), toForeign, readString)
import Data.Foreign.Class (readJSON)
import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (maybe)
import qualified Data.String as S

import TodomvcF.TodomvcF (TodomvcF(..), Todo(..))
import TodomvcF.StorageF (StorageF(), get, set)
import TodomvcF.ThisF (ThisF(), ForeignThis(), read, extend)

type AppF = Coproduct StorageF (ThisF AppT)

type App = FreeC AppF

newtype AppT = AppT { remainingCount :: Number
                    , todos :: [Todo]
                    , newTodo :: String }

key = "todos-angularjs-perf-purs-f"

todomvcN :: ForeignThis AppT -> Natural TodomvcF App
todomvcN this (Add (Todo t) a) =
  let title = S.trim t.title
      empty = (S.length title == 0)
      parse = \r -> either (const []) id (readString r >>= readJSON)
   in do
     todos <- injC $ maybe [] parse <$> get key
     let todos' = [Todo { title: title, completed: false }] <> todos
     injC $ set key (toForeign todos')
     AppT this' <- injC $ read this
     injC $ extend (AppT { remainingCount: this'.remainingCount + 1
                         , todos: todos'
                         , newTodo: "" }) this
     return a
