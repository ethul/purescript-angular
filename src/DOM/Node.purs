module DOM.Node where

import Control.Monad.Eff

foreign import data Node :: *

foreign import data DOM :: !

foreign import focus
  " function focus(node) { \
  \   return function(){ \
  \     return node.focus(); \
  \   }; \
  \ }"
  :: forall e. Node -> Eff (dom :: DOM | e) Unit
