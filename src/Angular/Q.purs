module Angular.Q
  ( Q(..)
  , Promise(..)
  , resolve
  , reject
  , when
  , all
  , then'
  , then''
  , then'''
  , catch'
  , finally'
  ) where

import Control.Monad.Eff

import Angular.Injector (InjectDependency(..))

foreign import data Q :: *

foreign import data Promise :: * -> *

instance functorPromise :: Functor Promise where
  (<$>) = liftA1

instance applyPromise :: Apply Promise where
  (<*>) = ap

instance applicativePromise :: Applicative Promise where
  pure = resolvePure

instance bindPromise :: Bind Promise where
  (>>=) = then'

instance monadPromise :: Monad Promise

foreign import injQ
  " function injQ(){ \
  \   var $injector = angular.element(document).injector(); \
  \   return $injector.get('$q'); \
  \ } "
  :: forall e. Eff (nginj :: InjectDependency | e) Q

foreign import resolvePure
  " function resolvePure(a){ \
  \   var $q = angular.injector(['ng']).get('$q') \
  \     , dfd = $q.defer() \
  \   ; \
  \   dfd.resolve(a); \
  \   return dfd.promise; \
  \ } "
  :: forall a. a -> Promise a

foreign import resolve
  " function resolve(a){ \
  \   return function($q){ \
  \     var dfd = $q.defer(); \
  \     dfd.resolve(a); \
  \     return dfd.promise; \
  \   }; \
  \ } "
  :: forall a. a -> Q -> Promise a

foreign import reject
  " function reject(e){ \
  \   return function($q){ \
  \     return $q.reject(e); \
  \   }; \
  \ } "
  :: forall a. a -> Q -> Promise a

foreign import when
  " function when(a){ \
  \   return function($q){ \
  \     return $q.when(a); \
  \   }; \
  \ } "
  :: forall a. a -> Q -> Promise a

foreign import all
  " function all(a){ \
  \   return function($q){ \
  \     return $q.all(a); \
  \   }; \
  \ } "
  :: forall a. [Promise a] -> Q -> Promise [a]

foreign import then'
  "function then$prime(fa){\
  \  return function(f){\
  \    return fa['then'](f);\
  \  };\
  \}"
  :: forall a b. Promise a -> (a -> Promise b) -> Promise b

foreign import then''
  "function then$prime$prime(fa){\
  \  return function(f){\
  \    return function(g){\
  \      return fa['then'](f, g);\
  \    };\
  \  };\
  \}"
  :: forall a b c. Promise a -> (a -> Promise b) -> (c -> Promise b) -> Promise b

foreign import then'''
  "function then$prime$prime$prime(fa){\
  \  return function(f){\
  \    return function(g){\
  \      return function(h){\
  \        return fa['then'](f, g, h);\
  \      };\
  \    };\
  \  };\
  \}"
  :: forall a b c d
  .  Promise a
  -> (a -> Promise b)
  -> (b -> Promise b)
  -> (c -> d)
  -> Promise b

foreign import catch'
  "function catch$prime(fa){\
  \  return function(f){\
  \    return fa['catch'](f);\
  \  };\
  \}"
  :: forall a b c. Promise a -> (b -> Promise c) -> Promise c

foreign import finally'
  "function finally$prime(fa){\
  \  return function(f){\
  \    return fa['finally'](f);\
  \  };\
  \}"
  :: forall a b. Promise a -> (Unit -> b) -> Promise a
