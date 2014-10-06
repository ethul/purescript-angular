module Todomvc.Main where

import Angular.Module (controller, directive, ngmodule')
import Angular.DI (annotate)

import Todomvc.Controller (todoctrl)
import Todomvc.Escape (escape)
import Todomvc.Focus (focus)

main = do
  m <- ngmodule' "todomvc" []
  controller "TodoCtrl" (annotate todoctrl) m
  directive "todoEscape" escape m
  directive "todoFocus" focus m
