module Angular.Deferred
  ( Deferred()
  , NgDeferred()
  , DeferredEff()
  , resolve
  , reject
  , notify
  , promise
  ) where

import Control.Monad.Eff
import Data.Function

import Angular.Promise (Promise())

foreign import data Deferred :: * -> * -> *

foreign import data NgDeferred :: !

type DeferredEff e r = Eff (ngdeferred :: NgDeferred | e) r

foreign import resolveFn
  " function resolveFn(a, dfd){ \
  \   return function(){ \
  \     dfd.resolve(a); \
  \   }; \
  \ } "
  :: forall e a b. Fn2 b (Deferred a b) (DeferredEff e Unit)

resolve :: forall e a b. b -> Deferred a b -> DeferredEff e Unit
resolve = runFn2 resolveFn

foreign import rejectFn
  " function rejectFn(a, dfd){ \
  \   return function(){ \
  \     dfd.reject(a); \
  \   }; \
  \ } "
  :: forall e a b. Fn2 a (Deferred a b) (DeferredEff e Unit)

reject :: forall e a b. a -> Deferred a b -> DeferredEff e Unit
reject = runFn2 rejectFn

foreign import notifyFn
  " function notifyFn(a, dfd){ \
  \   return function(){ \
  \     dfd.notify(a); \
  \   }; \
  \ } "
  :: forall e s a b. Fn2 s (Deferred a b) (DeferredEff e Unit)

notify :: forall e s a b. s -> Deferred a b -> DeferredEff e Unit
notify = runFn2 notifyFn

foreign import promise
  " function promise(dfd){ \
  \   return function(){ \
  \     dfd.promise; \
  \   }; \
  \ } "
  :: forall e a b. Deferred a b -> DeferredEff e (Promise a b)
