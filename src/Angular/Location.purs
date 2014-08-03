module Angular.Location
  ( Location()
  , Loc()
  , LocEff()
  , getPath
  , setPath
  ) where

import Control.Monad.Eff

foreign import data Location :: *

foreign import data Loc :: !

type LocEff e a = Eff (ngloc :: Loc | e) a

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
