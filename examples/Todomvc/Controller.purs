module Todomvc.Controller (todoctrl) where

import qualified Data.Array as A
import qualified Data.String as S
import Data.Foldable
import Data.Maybe
import Control.Apply ((*>))
import Control.Monad (unless, when)
import Control.Monad.Eff

import Angular (copy, extend)
import Angular.Location (Location(), getPath, setPath)
import Angular.Scope (Scope(), watch, readScope, extendScope, modifyScope)
import Angular.This (readThis, writeThis)

import Todomvc.Storage (Store(), Todo(), get, put)

addTodo scope = do
  s <- readScope scope
  let title = S.trim s.newTodo
  let empty = (S.length title) == 0
  unless empty $ do
    let todo = { title: title, completed: false }
    todos <- get
    let res = todos <> [todo]
    put res
    extendScope { todos: res
                , newTodo: ""
                , remainingCount: s.remainingCount + 1 } scope

todoCompleted scope todo
  = modifyScope (\s -> do
      let change = if todo.completed then -1 else 1
      put s.todos
      return { remainingCount: s.remainingCount + change }
    ) scope

clearCompletedTodos scope = do
  s <- readScope scope
  let res = A.filter (\a -> not a.completed) s.todos
  put res
  extendScope { todos: res } scope

markAll scope compl
  = modifyScope (\s -> do
      let res = (\a -> a { completed = not compl }) <$> s.todos
      put res
      return { todos: res
             , remainingCount: if compl then A.length res else 0 }
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
      put s.todos
      return { editedTodo: Nothing }
    ) scope

removeTodo scope todo = do
  s <- readScope scope
  let res = A.filter (\a -> not $ refEq a todo) s.todos
  unless (A.length res == A.length s.todos) do
    let c = if todo.completed then 0 else -1
    extendScope { todos: res
                , remainingCount: s.remainingCount + c } scope
    put res

revertEditing scope todo = do
  s <- readScope scope
  let res = A.filter (\a -> not $ refEq a todo) s.todos
  unless (A.length res == A.length s.todos) do
    case s.originalTodo of
      Just t  -> extendScope { todos: res <> [t] } scope *>
                 doneEditing scope t
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

todoctrl scope this location = do
  t <- readThis this
  path <- getPath location
  if S.length path == 0 then setPath "/" location else return ""
  watchRemainingCount scope
  watchLocationPath scope
  todos <- get
  let remainingCount = foldl (\b a -> if a.completed then b else b + 1) 0 todos
  extendScope { fromMaybe: fromMaybe
              , newTodo: ""
              , editedTodo: Nothing
              , originalTodo: Nothing
              , todos: todos
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
