module Todomvc.Focus (FocusDirective(), focus) where

import Data.Maybe
import Control.Monad (when)
import Control.Monad.Eff

import qualified DOM.Node as D

import Angular.Attributes (get)
import Angular.Element ((!!))
import Angular.Scope (watch, apply, applyExpr)
import Angular.Timeout (timeout)

foreign import data FocusDirective :: *

link scope element attrs ngtimeout = do
  as <- get attrs
  let k = maybe (return unit) D.focus (element !! 0)
  watch as.todoFocus (Just (\a _ _ -> when a $ const unit <$> (timeout k 0 false ngtimeout))) false scope

foreign import focus
  " /*@ngInject*/function focus($timeout){ \
  \   return { \
  \     link: function($scope, $element, $attrs){ \
  \       return link($scope)($element)($attrs)($timeout)(); \
  \     } \
  \   }; \
  \ } " :: FocusDirective
