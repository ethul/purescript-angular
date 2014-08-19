module Angular.Cache
 ( Cache()
 , CacheFactory()
 , CacheEff()
 , NgCache()
 , Key()
 , Name()
 , Options()
 , cache
 , put
 , get
 , remove
 , removeAll
 , destroy
 , info
 ) where

import Control.Monad.Eff
import Data.Maybe
import Data.Function

foreign import data NgCache :: !

foreign import data Cache :: *

foreign import data CacheFactory :: *

type Key = String

type Name = String

type Options a = { capacity :: Number | a }

type CacheEff e r = Eff (ngcache :: NgCache | e) r

cache :: forall e a. Name -> Maybe (Options a) -> CacheFactory -> CacheEff e Cache
cache = runFn4 cacheFn fromMaybe

foreign import cacheFn
  " function cacheFn(fromMaybe, name, opts, $cacheFactory){ \
  \   return function(){ \
  \     return $cacheFactory(name, fromMaybe(undefined)(opts)); \
  \   }; \
  \ } "
  :: forall e a. Fn4 (Options a -> Maybe (Options a) -> Options a)
                     Name
                     (Maybe (Options a))
                     CacheFactory
                     (CacheEff e Cache)

foreign import put
  "  function put(key){ \
  \    return function(value){ \
  \      return function(cache){ \
  \        return function(){ \
  \          return cache.put(key, value); \
  \        }; \
  \      }; \
  \    }; \
  \  } "
  :: forall e a. Key -> a -> Cache -> CacheEff e a

foreign import get
  "  function get(key){ \
  \    return function(cache){ \
  \      return function(){ \
  \        return cache.get(key); \
  \      }; \
  \    }; \
  \  } "
  :: forall e a. Key -> Cache -> CacheEff e a

foreign import remove
  "  function remove(key){ \
  \    return function(cache){ \
  \      return function(){ \
  \        return cache.remove(key); \
  \      }; \
  \    }; \
  \  } "
  :: forall e. Key -> Cache -> CacheEff e Unit

foreign import removeAll
  "  function removeAll(cache){ \
  \    return function(){ \
  \      return cache.removeAll(); \
  \    }; \
  \  } "
  :: forall e. Cache -> CacheEff e Unit

foreign import destroy
  "  function destroy(cache){ \
  \    return function(){ \
  \      return cache.destroy(); \
  \    }; \
  \  } "
  :: forall e. Cache -> CacheEff e Unit

foreign import info
  "  function info(cache){ \
  \    return function(){ \
  \      return cache.info(); \
  \    }; \
  \  } "
  :: forall e a. Cache -> CacheEff e { id :: String, size :: Number | a }
