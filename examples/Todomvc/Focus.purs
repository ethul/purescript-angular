module Todomvc.Focus where

import Data.Maybe
import Debug.Trace (trace)
import Control.Monad (when)
import Control.Monad.Eff

import DOM.Node (DOM(..), focus)

import Angular.Attributes (Attr(..), get)
import Angular.Directive (LinkFn(..), directive, defn, postLink)
import Angular.Element ((!!))
import Angular.Scope (watch, apply, applyExpr)

focusLink :: forall e a b c d. LinkFn (ngattr :: Attr, dom :: DOM | e) a b c d
focusLink scope element attrs ctrls trans = do
  as <- get attrs
  watch as.todoFocus (Just (\a1 _ _ -> do
                             when a1 $ do
                               zeroTimeout case element !! 0 of
                                             Just el -> focus el
                                             Nothing -> return unit
                               return unit
                           )) false scope
  return unit

foreign import zeroTimeout
  " function zeroTimeout(k) { \
  \   return function(){ \
  \     var $timeout = angular.element(document).injector().get('$timeout'); \
  \     return $timeout(k, 0, false); \
  \   }; \
  \ }"
  :: forall e. Eff e Unit -> Eff e Unit

focusDirective m =
  directive "todoFocus" m $ return defn { link = postLink focusLink }
