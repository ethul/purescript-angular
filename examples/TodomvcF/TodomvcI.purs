module TodomvcF.TodomvcI
  ( AppT()
  , AppE()
  , run
  ) where

import Control.Alt ((<|>))
import Control.Monad.Eff
import Control.Monad.Free (FreeC(), injC, goEffC, goMC)
import Data.Coyoneda (Natural())
import Data.Either (either)
import Data.Foreign (F(), toForeign, readString)
import Data.Foreign.Class (readJSON)
import Data.Functor.Coproduct (Coproduct())
import Data.Inject (prj)
import Data.Maybe (maybe)
import Data.Maybe.Unsafe (fromJust)
import qualified Data.String as S

import TodomvcF.TodomvcF (Todomvc(), TodomvcF(..), Todo(..))
import TodomvcF.StorageF (StorageF(), get, set)
import TodomvcF.StorageI (Store(), storageN)
import TodomvcF.ThisF (ThisF(), ForeignThis(), read, extend)
import TodomvcF.ThisI (FThis(), thisN)

type AppF = Coproduct StorageF (ThisF AppT)

type AppE e = Eff (store :: Store, this :: FThis | e)

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
     let todos' = [Todo t { title = title }] <> todos
     injC $ set key (toForeign todos')
     AppT this' <- injC $ read this
     injC $ extend (AppT { remainingCount: this'.remainingCount + 1
                         , todos: todos'
                         , newTodo: "" }) this
     return a

appN :: forall e. Natural AppF (AppE e)
appN fa = fromJust $ (storageN <$> prj fa) <|>
                     ((thisN :: forall e. Natural (ThisF AppT)
                                                  (Eff (this :: FThis | e))) <$> prj fa)

runTodomvc :: forall a. ForeignThis AppT -> Todomvc a -> App a
runTodomvc t = goMC (todomvcN t)

runApp :: forall e a. App a -> AppE e a
runApp = goEffC appN

run :: forall e a. ForeignThis AppT -> Todomvc a -> AppE e a
run t = runApp <<< (runTodomvc t)
