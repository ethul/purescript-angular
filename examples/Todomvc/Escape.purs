module Todomvc.Escape where

import Data.Maybe

import Angular.Attributes (get)
import Angular.Directive (LinkFn(..), directive, defn, postLink)
import Angular.Element (El(..), on)
import Angular.Scope (apply, stringApplyExpr)

escapeKey = 27

escapeLink :: forall e a b c d. LinkFn (ngel :: El | e) a b c d
escapeLink scope element attrs ctrls trans = do
  on "keydown" (\e -> if e.keyCode == escapeKey
                      then (get attrs) >>= (\as -> apply (stringApplyExpr as.todoEscape) scope)
                      else return unit) element
  return unit

escapeDirective m =
  directive "todoEscape" m $ return defn { link = postLink escapeLink }
