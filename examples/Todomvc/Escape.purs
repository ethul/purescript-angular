module Todomvc.Escape (EscapeDirective(), escape) where

import Data.Maybe

import Angular.Attributes (get)
import Angular.Element (on)
import Angular.Scope (apply, stringApplyExpr)

foreign import data EscapeDirective :: *

escapeKey = 27

link scope element attrs = do
  on "keydown" (\e -> if e.keyCode == escapeKey
                      then (get attrs) >>= (\as -> apply (stringApplyExpr as.todoEscape) scope)
                      else return unit) element

foreign import escape
  " /*@ngInject*/function escape(){ \
  \   return { \
  \     link: function($scope, $element, $attrs){ \
  \       return link($scope)($element)($attrs)(); \
  \     } \
  \   }; \
  \ } " :: EscapeDirective
