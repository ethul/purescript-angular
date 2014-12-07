module TodomvcF.Main where

import Control.Monad.Eff (Eff())
import Data.Array (filter, length)
import Data.Maybe (Maybe(..), fromMaybe)
import qualified Data.String as S

import Angular.Module (controller, directive, ngmodule')
import Angular.Location (getPath, setPath)
import Angular.This (readThis, extendThis)

import TodomvcF.AppI (run)
import TodomvcF.Escape (escape)
import TodomvcF.Focus (focus)
import TodomvcF.ThisF (ForeignThis())
import TodomvcF.TodomvcF (Todo(..), add, all, remove, edit, revert, clearCompleted, markAll, completed, doneEdit)
import TodomvcF.TodomvcI (AppT(..))

mainctrl this location =
  let app = { newTodo: ""
            , todos: []
            , remainingCount: 0
            , originalTodo: Nothing :: Maybe Todo
            , editedTodo: Nothing :: Maybe Todo }
   in do
     getPath location >>= (\p -> if S.length p == 0 then setPath "/" location else pure "")
     todos <- run (toForeignThis $ AppT app) all
     let app' = app { todos = todos
                    , remainingCount = length (filter (\(Todo t) -> not t.completed) todos) }
     extendThis { add: add
                , all: all
                , remove: remove
                , edit: edit
                , revert: revert
                , clearCompleted: clearCompleted
                , markAll: markAll
                , completed: completed
                , doneEdit: doneEdit
                , statusFilter: statusFilter
                , location: location
                , fromMaybe: fromMaybe
                , run: (run <<< toForeignThis <<< AppT) app'
                , app: app' } this
  where
    statusFilter = (\path -> case path of
                                  "/completed" -> Just { completed: true }
                                  "/active" -> Just { completed: false }
                                  _ -> Nothing) <$> getPath location

main = do
  m <- ngmodule' "todomvcf" []
  controller "Main" mainctrl' m
  directive "todoEscape" escape m
  directive "todoFocus" focus m

foreign import toForeignThis """
  function toForeignThis(that){
    return that;
  }
""" :: AppT -> ForeignThis AppT

foreign import mainctrl' """
  /*@ngInject*/function mainctrl$prime($location) {
    var impl = mainctrl(this)($location);
    return impl.apply(this, []);
  }
""" :: forall e a. Eff e Unit
