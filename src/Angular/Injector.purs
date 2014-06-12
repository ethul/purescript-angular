module Angular.Injector where

import Control.Monad.Eff

foreign import data Injector :: *

foreign import data InjectDependency :: !

foreign import injector
  " function injector(modules){ \
  \   return function(){ \
  \     return angular.injector(modules); \
  \   }; \
  \ }"
  :: forall e. Eff e Injector
