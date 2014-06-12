module Angular.Location where

import Control.Monad.Eff

import Angular.Injector (InjectDependency(..))

foreign import data Location :: *

foreign import data Loc :: !

foreign import location
  " function location(){ \
  \   var $injector = angular.element(document).injector(); \
  \   return $injector.get('$location'); \
  \ }"
  :: forall e. Eff (nginj :: InjectDependency | e) Location

foreign import getPath
  " function getPath(loc){ \
  \   return function(){ \
  \     return loc.path(); \
  \   }; \
  \ }"
  :: forall e. Location -> Eff (ngloc :: Loc | e) String

foreign import setPath
  " function setPath(path){ \
  \   return function(loc){ \
  \     return function(){ \
  \       return loc.path(path); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Location -> Eff (ngloc :: Loc | e) String
