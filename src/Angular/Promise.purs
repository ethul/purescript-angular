module Angular.Promise
  ( Promise(..)
  , then'
  , then''
  , then'''
  , catch'
  , finally'
  , pureResolve
  , pureReject
  ) where

import Control.Monad.Eff
import Data.Bifunctor
import Data.Function

foreign import data Promise :: * -> * -> *

instance functorPromise :: Functor (Promise a) where
  (<$>) = liftA1

instance applyPromise :: Apply (Promise a) where
  (<*>) = ap

instance applicativePromise :: Applicative (Promise a) where
  pure = pureResolve

instance bindPromise :: Bind (Promise a) where
  (>>=) = flip then'

instance monadPromise :: Monad (Promise a)

instance bifunctorPromise :: Bifunctor Promise where
  bimap f g = then'' (pureResolve <<< g) (pureReject <<< f)

foreign import pureResolve
  " function pureResolve(a){ \
  \   var $q = angular.injector(['ng']).get('$q') \
  \     , dfd = $q.defer() \
  \   ; \
  \   dfd.resolve(a); \
  \   return dfd.promise; \
  \ } "
  :: forall a b. b -> Promise a b

foreign import pureReject
  " function pureReject(a){ \
  \   var $q = angular.injector(['ng']).get('$q') \
  \     , dfd = $q.defer() \
  \   ; \
  \   dfd.reject(a); \
  \   return dfd.promise; \
  \ } "
  :: forall a b. a -> Promise a b

foreign import thenFn'
  " function thenFn$prime(f, fa){ \
  \   return fa['then'](f); \
  \ } "
  :: forall a b c d. Fn2 (b -> Promise c d) (Promise a b) (Promise c d)

then' :: forall a b c. (b -> Promise a c) -> Promise a b -> Promise a c
then' = runFn2 thenFn'

foreign import thenFn''
  " function thenFn$prime$prime(f, g, fa){ \
  \   return fa['then'](f, g); \
  \ } "
  :: forall a b c d. Fn3 (b -> Promise c d) (a -> Promise c d) (Promise a b) (Promise c d)

then'' :: forall a b c d. (b -> Promise c d) -> (a -> Promise c d) -> Promise a b -> Promise c d
then'' = runFn3 thenFn''

foreign import thenFn'''
  " function thenFn$prime$prime$prime(f, g, h, fa){ \
  \   return fa['then'](f, g, function(a){return h(a)();}); \
  \ } "
  :: forall e s t a b c d. Fn4 (b -> Promise c d)
                               (a -> Promise c d)
                               (s -> Eff e t)
                               (Promise a b)
                               (Promise c d)

then''' :: forall e s t a b c d. (b -> Promise c d) -> (a -> Promise c d) -> (s -> Eff e t) -> Promise a b -> Promise c d
then''' = runFn4 thenFn'''

foreign import catchFn'
  " function catchFn$prime(f, fa){ \
  \   return fa['catch'](f); \
  \ } "
  :: forall a b c d. Fn2 (a -> Promise c d) (Promise a b) (Promise c d)

catch' :: forall a b c d. (a -> Promise c d) -> Promise a b -> Promise c d
catch' = runFn2 catchFn'

foreign import finallyFn'
  " function finallyFn$prime(f, fa){ \
  \   return fa['finally'](f); \
  \ } "
  :: forall e r a b. Fn2 (Eff e r) (Promise a b) (Promise a b)

finally' :: forall e r a b. Eff e r -> Promise a b -> Promise a b
finally' = runFn2 finallyFn'
