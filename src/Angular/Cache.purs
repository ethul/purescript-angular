module Angular.Cache
 ( Cache()
 , CacheFactory()
 , CACHE()
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

foreign import data CACHE :: !

foreign import data Cache :: *

foreign import data CacheFactory :: *

type Key = String

type Name = String

type Options a = { capacity :: Number | a }

foreign import cache
  " function cache(name){ \
  \   return function(opts){ \
  \     return function($cacheFactory){ \
  \       return function(){ \
  \         return $cacheFactory(name, opts.values[0]); \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a. Name -> Maybe (Options a) -> CacheFactory -> Eff (ngcache :: CACHE | e) Cache

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
  :: forall e a. Key -> a -> Cache -> Eff (ngcache :: CACHE | e) a

foreign import get
  "  function get(key){ \
  \    return function(cache){ \
  \      return function(){ \
  \        return cache.get(key); \
  \      }; \
  \    }; \
  \  } "
  :: forall e a. Key -> Cache -> Eff (ngcache :: CACHE | e) a

foreign import remove
  "  function remove(key){ \
  \    return function(cache){ \
  \      return function(){ \
  \        return cache.remove(key); \
  \      }; \
  \    }; \
  \  } "
  :: forall e a. Key -> Cache -> Eff (ngcache :: CACHE | e) Unit

foreign import removeAll
  "  function removeAll(cache){ \
  \    return function(){ \
  \      return cache.removeAll(); \
  \    }; \
  \  } "
  :: forall e. Cache -> Eff (ngcache :: CACHE | e) Unit

foreign import destroy
  "  function destroy(cache){ \
  \    return function(){ \
  \      return cache.destroy(); \
  \    }; \
  \  } "
  :: forall e. Cache -> Eff (ngcache :: CACHE | e) Unit

foreign import info
  "  function info(cache){ \
  \    return function(){ \
  \      return cache.info(); \
  \    }; \
  \  } "
  :: forall e a. Cache -> Eff (ngcache :: CACHE | e) { id :: String, size :: Number | a }
