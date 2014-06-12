module Todomvc.Main where

import Angular.Module (newModule)

import Todomvc.Controller (todoController)
import Todomvc.Escape (escapeDirective)
import Todomvc.Focus (focusDirective)

main = do
  m <- newModule "todomvc" []
  todoController m
  escapeDirective m
  focusDirective m
