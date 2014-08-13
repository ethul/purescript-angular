module Todomvc.Controller (todoctrl) where

import qualified Data.Array as A
import qualified Data.String as S
import Data.Foldable
import Data.Maybe
import Control.Monad (unless, when)
import Control.Monad.Eff
import Control.Monad.ST (ST(), STArray(), newSTArray, runSTArray)

import Angular (copy, extend)
import Angular.Location (Location(), getPath, setPath)
import Angular.Scope (Scope(), watch, readScope, extendScope, modifyScope)
import Angular.This(readThis, writeThis)
import Angular.ST (readSTArray, pushSTArray, pushAllSTArray, writeSTArray, spliceSTArray)

import Todomvc.Storage (Store(), Todo(), get, put)

addTodo scope = do
  s <- readScope scope
  let title = S.trim s.newTodo
  let empty = (S.length title) == 0
  unless empty $ do
    let todo = { title: title, completed: false }
    todos <- get
    put $ todos <> [todo]
    pushSTArray s.todos todo
    extendScope { newTodo: ""
                , remainingCount: s.remainingCount + 1 } scope

todoCompleted scope todo
  = modifyScope (\s -> do
      let change = if todo.completed then -1 else 1
      arr <- readSTArray s.todos
      put arr
      return { remainingCount: s.remainingCount + change }
    ) scope

clearCompletedTodos scope = do
  s <- readScope scope
  arr <- readSTArray s.todos
  let res = A.filter (\a -> not a.completed) arr
  put res
  writeSTArray s.todos res

markAll scope compl
  = modifyScope (\s -> do
      arr <- readSTArray s.todos
      let res = (\a -> a { completed = not compl }) <$> arr
      put res
      writeSTArray s.todos res
      return { remainingCount: if compl then A.length res else 0 }
    ) scope

editTodo scope todo = do
  cp <- copy todo
  extendScope { editedTodo: Just todo
              , originalTodo: Just $ cp } scope

doneEditing scope todo
  = modifyScope (\s -> do
      let title = S.trim todo.title
      when (S.length title == 0) $ removeTodo scope todo
      extend todo { title: title }
      arr <- readSTArray s.todos
      put arr
      return { editedTodo: Nothing }
    ) scope

removeTodo scope todo = do
  s <- readScope scope
  arr <- readSTArray s.todos
  let i = A.findIndex (\a -> refEq a todo) arr
  unless (i == -1) do
    let c = if todo.completed then 0 else -1
    extendScope { remainingCount: s.remainingCount + c} scope
    spliceSTArray s.todos i 1 []
    put arr

revertEditing scope todo = do
  s <- readScope scope
  arr <- readSTArray s.todos
  let i = A.findIndex (\a -> refEq a todo) arr
  unless (i == -1) do
    case s.originalTodo of
      Just t  -> do
        spliceSTArray s.todos i 1 [t]
        doneEditing scope t
        return unit
      Nothing -> return unit

watchRemainingCount scope =
  let expr = "remainingCount == 0"
      listener = \a _ _ -> extendScope { allChecked: a } scope
  in watch expr (return listener) false scope

watchLocationPath scope =
  let expr = "location.path()"
      listener = \a _ _ -> case a of
                                "/completed" -> extendScope { statusFilter: { completed: true } } scope
                                "/active"    -> extendScope { statusFilter: { completed: false } } scope
                                _            -> extendScope { statusFilter: { } } scope
  in watch expr (return listener) false scope

controller scope location = do
  path <- getPath location
  if S.length path == 0 then setPath "/" location else return ""
  watchRemainingCount scope
  watchLocationPath scope
  todosRef <- newSTArray 0 { title: "", completed: false }
  todos <- get
  pushAllSTArray todosRef todos
  let remainingCount = foldl (\b a -> if a.completed then b else b + 1) 0 todos
  extendScope { newTodo: ""
              , editedTodo: Nothing
              , originalTodo: Nothing
              , todos: todosRef
              , remainingCount: remainingCount
              , location: location
              , addTodo: addTodo scope
              , todoCompleted: todoCompleted scope
              , clearCompletedTodos: clearCompletedTodos scope
              , markAll: markAll scope
              , editTodo: editTodo scope
              , doneEditing: doneEditing scope
              , removeTodo: removeTodo scope
              , revertEditing: revertEditing scope } scope

foreign import todoctrl
  " /*@ngInject*/function todoctrl($scope, $location) { \
  \   var impl = controller($scope)($location); \
  \   return impl.apply(this, []); \
  \ } "
  :: forall e a. Scope a -> Location -> Eff e Unit
