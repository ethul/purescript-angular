module Todomvc.Controller where

import qualified Data.Array as A
import qualified Data.String as S
import Data.Foldable
import Data.Maybe
import Control.Monad (unless, when)
import Control.Monad.Eff
import Control.Monad.ST (ST(..), STArray(..), newSTArray, runSTArray)

import Angular (copy, extend)
import Angular.Controller (ReadScopeState(..), WriteScopeState(..), controller, readScopeState, writeScopeState, modifyScopeState)
import Angular.Location (location, getPath, setPath)
import Angular.Scope (Scope(..), watch)
import Angular.ST (readSTArray, pushSTArray, pushAllSTArray, writeSTArray, spliceSTArray)

import Todomvc.Storage (Store(..), Todo(..), get, put)

addTodo scope = do
  s <- readScopeState scope
  let title = S.trim s.newTodo
  let empty = (S.length title) == 0
  unless empty $ do
    let todo = { title: title, completed: false }
    todos <- get
    put $ todos <> [todo]
    pushSTArray s.todos todo
    writeScopeState { newTodo: "" , remainingCount: s.remainingCount + 1 } scope
    return unit

todoCompleted scope todo
  = modifyScopeState (\s -> do
      let change = if todo.completed then -1 else 1
      arr <- readSTArray s.todos
      put arr
      return { remainingCount: s.remainingCount + change }
    ) scope

clearCompletedTodos scope = do
  s <- readScopeState scope
  arr <- readSTArray s.todos
  let res = A.filter (\a -> not a.completed) arr
  put res
  writeSTArray s.todos res

markAll scope compl
  = modifyScopeState (\s -> do
      arr <- readSTArray s.todos
      let res = (\a -> a { completed = not compl }) <$> arr
      put res
      writeSTArray s.todos res
      return { remainingCount: if compl then A.length res else 0 }
    ) scope

editTodo scope todo = do
  cp <- copy todo
  writeScopeState { editedTodo: Just todo , originalTodo: Just $ cp } scope

doneEditing scope todo
  = modifyScopeState (\s -> do
      let title = S.trim todo.title
      when (S.length title == 0) $ removeTodo scope todo
      extend todo { title: title }
      arr <- readSTArray s.todos
      put arr
      return { editedTodo: Nothing }
    ) scope

removeTodo scope todo = do
  s <- readScopeState scope
  arr <- readSTArray s.todos
  let i = A.findIndex (\a -> refEq a todo) arr
  unless (i == -1) do
    let c = if todo.completed then 0 else -1
    writeScopeState { remainingCount: s.remainingCount + c} scope
    spliceSTArray s.todos i 1 []
    put arr

revertEditing scope todo = do
  s <- readScopeState scope
  arr <- readSTArray s.todos
  let i = A.findIndex (\a -> refEq a todo) arr
  unless (i == -1) do
    case s.originalTodo of
      Just t  -> do
        spliceSTArray s.todos i 1 [t]
        doneEditing scope t
        return unit
      Nothing -> return unit

watchRemainingCount scope = do
  watch "remainingCount == 0" (Just (\a1 _ _ ->
    writeScopeState { allChecked: a1 } scope >>= (\_ -> return unit))) false scope

watchLocationPath scope = do
  watch "location.path()" (Just (\a1 _ _ ->
    case a1 of
      "/completed" -> writeScopeState { statusFilter: {completed: true} } scope >>= (\_ -> return unit)
      "/active"    -> writeScopeState { statusFilter: {completed: false} } scope >>= (\_ -> return unit)
      _            -> writeScopeState { statusFilter: {} } scope >>= (\_ -> return unit)
    )) false scope

todoController m =
  controller "TodoCtrl" m $ \scope _ -> do
    loc <- location
    path <- getPath loc
    if S.length path == 0 then setPath "/" loc else return ""
    watchRemainingCount scope
    watchLocationPath scope
    todosRef <- newSTArray 0 { title: "", completed: false }
    todos <- get
    pushAllSTArray todosRef todos
    let remainingCount = foldl (\b a -> if a.completed then b else b + 1) 0 todos
    writeScopeState { newTodo: ""
                    , editedTodo: Nothing
                    , originalTodo: Nothing
                    , todos: todosRef
                    , remainingCount: remainingCount
                    , location: loc
                    , addTodo: addTodo scope
                    , todoCompleted: todoCompleted scope
                    , clearCompletedTodos: clearCompletedTodos scope
                    , markAll: markAll scope
                    , editTodo: editTodo scope
                    , doneEditing: doneEditing scope
                    , removeTodo: removeTodo scope
                    , revertEditing: revertEditing scope } scope
