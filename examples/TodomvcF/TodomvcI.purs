module TodomvcF.TodomvcI
  ( AppF()
  , App()
  , AppT(..)
  , todomvcN
  ) where

import Control.Alt ((<|>))
import Control.Monad (unless, when)
import Control.Monad.Eff (Eff())
import Control.Monad.Free (FreeC(), injC)
import Data.Coyoneda (Natural())
import Data.Either (either)
import Data.Foreign (F(), toForeign, readString)
import Data.Foreign.Class (readJSON)
import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe(..), maybe)
import qualified Data.Array as A
import qualified Data.String as S

import TodomvcF.TodomvcF (TodomvcF(..), Todo(..))
import TodomvcF.StorageF (StorageF(), get, set)
import TodomvcF.ThisF (ThisF(), ForeignThis(), read, extend)

newtype AppT = AppT { remainingCount :: Number
                    , todos :: [Todo]
                    , newTodo :: String
                    , originalTodo :: Maybe Todo
                    , editedTodo :: Maybe Todo }

type AppF = Coproduct StorageF (ThisF AppT)

type App = FreeC AppF

todomvcN :: ForeignThis AppT -> Natural TodomvcF App
todomvcN this fa =
  case fa of
       Add (Todo t) a -> let title = S.trim t.title
                             empty = (S.length title == 0)
                          in const a <$> unless empty do
                            todos <- getOrEmpty
                            let todos' = [Todo t { title = title }] <> todos
                            setTodos todos'
                            AppT this' <- injC $ read this
                            extendApp (this' { remainingCount = this'.remainingCount + 1
                                             , todos = todos'
                                             , newTodo = "" }) this

       Remove (Todo t) a -> const a <$> remove t

       Edit (Todo t) a -> do
         AppT this' <- injC $ read this
         extendApp (this' { originalTodo = (Just <<< Todo) t { title = t.title }
                          , editedTodo = (Just <<< Todo) t }) this
         return a

       DoneEdit (Todo t) a -> let title = S.trim t.title
                                  empty = (S.length title == 0)
                               in do
                                 AppT this' <- injC $ read this
                                 if empty then remove t else setTodos this'.todos
                                 extendApp (this' { editedTodo = Nothing }) this
                                 return a

       Revert (Todo t) a -> do
         AppT this' <- injC $ read this
         let res = A.filter (not <<< refEq t <<< todoi) this'.todos
             n = A.length res
             m = A.length this'.todos
         const a <$>
         unless (n == m) (maybe (return unit)
                                (\t -> let todos = [t] <> res
                                        in do
                                          setTodos todos
                                          extendApp (this' { todos = todos }) this
                                          return unit) this'.originalTodo)

       All k -> k <$> getOrEmpty

       ClearCompleted a -> do
         AppT this' <- injC $ read this
         let completed = A.filter (not <<< completedi) this'.todos
         setTodos completed
         extendApp (this' { remainingCount = this'.remainingCount
                          , todos = completed }) this
         return a

       MarkAll v a -> do
         AppT this' <- injC $ read this
         let marked = (\(Todo a) -> Todo $ a { completed = not v }) <$> this'.todos
         setTodos marked
         extendApp (this' { remainingCount = if v then A.length marked else 0
                          , todos = marked }) this
         return a

       Completed (Todo t) a -> do
         AppT this' <- injC $ read this
         setTodos this'.todos
         extendApp (this' { remainingCount = this'.remainingCount +
                                             if t.completed then -1 else 1 }) this
         return a

  where
    key = "todos-angularjs-perf-purs-f"
    completedi (Todo a) = a.completed
    todoi (Todo a) = a
    parse r = either (const []) id (readString r >>= readJSON)
    getOrEmpty = injC $ maybe [] parse <$> get key
    setTodos as = injC $ set key (toForeign as)
    extendApp a t = injC $ extend (AppT a) t
    remove t = do
      AppT this' <- injC $ read this
      let res = A.filter (not <<< refEq t <<< todoi) this'.todos
          n = A.length res
          m = A.length this'.todos
      unless (n == m) do
        setTodos res
        extendApp (this' { remainingCount = this'.remainingCount + if t.completed then 0 else -1
                         , todos = res }) this
