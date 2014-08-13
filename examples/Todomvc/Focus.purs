module Todomvc.Focus (FocusDirective(), focus) where

import Data.Maybe
import Control.Monad (when)
import Control.Monad.Eff

import qualified DOM.Node as D

import Angular.Attributes (Attr(), get)
import Angular.Element ((!!))
import Angular.Scope (watch, apply, applyExpr)

foreign import data FocusDirective :: *

link scope element attrs = do
  as <- get attrs
  watch as.todoFocus (Just (\a _ _ -> when a $ zeroTimeout
                                             $ maybe (return unit) D.focus (element !! 0))) false scope

foreign import zeroTimeout
  " function zeroTimeout(k) { \
  \   return function(){ \
  \     var $timeout = angular.element(document).injector().get('$timeout'); \
  \     return $timeout(k, 0, false); \
  \   }; \
  \ }"
  :: forall e. Eff e Unit -> Eff e Unit

foreign import focus
  " /*@ngInject*/function focus(){ \
  \   return { \
  \     link: function($scope, $element, $attrs){ \
  \       return link($scope)($element)($attrs)(); \
  \     } \
  \   }; \
  \ } " :: FocusDirective
