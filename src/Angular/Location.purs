module Angular.Location
  ( Location()
  , LocEff()
  , NgLoc()
  , getPath
  , setPath
  ) where

import Control.Monad.Eff

foreign import data Location :: *

foreign import data NgLoc :: !

type LocEff e a = Eff (ngloc :: NgLoc | e) a

foreign import getPath
  " function getPath(loc){ \
  \   return function(){ \
  \     return loc.path(); \
  \   }; \
  \ }"
  :: forall e. Location -> LocEff e String

foreign import setPath
  " function setPath(path){ \
  \   return function(loc){ \
  \     return function(){ \
  \       return loc.path(path); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Location -> LocEff e String
