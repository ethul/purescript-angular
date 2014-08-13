module Todomvc.Main where

import Angular.Module (controller, directive, ngmodule')

import Todomvc.Controller (todoctrl)
import Todomvc.Escape (escape)
import Todomvc.Focus (focus)

main = do
  m <- ngmodule' "todomvc" []
  controller "TodoCtrl" todoctrl m
  directive "todoEscape" escape m
  directive "todoFocus" focus m
