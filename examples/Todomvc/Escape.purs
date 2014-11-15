module Todomvc.Escape (EscapeDirective(), escape) where

import Data.Maybe
import Control.Bind ((>=>))

import Data.DOM.Simple.Events (keyCode)

import Angular.Attributes (get)
import Angular.Element (on)
import Angular.Scope (apply, stringApplyExpr)

foreign import data EscapeDirective :: *

escapeKey = 27

link scope element attrs = do
  on "keydown" (keyCode >=> \kc ->
                      if kc == escapeKey
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
