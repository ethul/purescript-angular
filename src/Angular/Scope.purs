module Angular.Scope
  ( Scope()
  , ScopeEff()
  , NgReadScope()
  , NgWriteScope()
  , NgScope()
  , ReadEff()
  , WriteEff()
  , ReadWriteEff()
  , WatchListener()
  , Event()
  , ApplyExpr(..)
  , WatchDeregistration()
  , OnDeregistration()
  , readScope
  , writeScope
  , extendScope
  , modifyScope
  , newScope
  , watch
  , watchCollection
  , digest
  , destroy
  , evalSync
  , evalAsync
  , applyExpr
  , stringApplyExpr
  , defaultApplyExpr
  , apply
  , on
  , emit
  , broadcast
  , deregisterWatch
  , deregisterOn
  , id
  , root
  , parent
  ) where

import Prelude (Unit(), ($))
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe
import Data.Maybe
import Data.Function (Fn2(), Fn3(), Fn4(), Fn5(), runFn2, runFn3, runFn4, runFn5)

type WatchListener e a b = a -> a -> Scope b -> Eff e Unit

type Event e a b
  = { targetScope :: Scope a
    , currentScope :: Scope b
    , name :: String
    , stopPropagation :: Eff e Unit
    , preventDefault :: Eff e Unit
    , defaultPrevented :: Boolean
    }

data ApplyExpr e r a
  = DefaultApplyExpr
  | StringApplyExpr String
  | FnApplyExpr (Scope a -> Eff e r)

foreign import data Scope :: # * -> *

foreign import data WatchDeregistration :: *

foreign import data OnDeregistration :: *

foreign import data NgReadScope :: !

foreign import data NgWriteScope :: !

foreign import data NgScope :: !

type ReadEff e a = Eff (ngrscope :: NgReadScope | e) { | a }

type WriteEff e = Eff (ngwscope :: NgWriteScope | e) Unit

type ReadWriteEff e r = Eff (ngrscope :: NgReadScope, ngwscope :: NgWriteScope | e) r

type ScopeEff e r = Eff (ngscope :: NgScope | e) r

modifyScope :: forall e a b. ({ | a} -> ReadWriteEff e { | b }) -> Scope a -> ReadWriteEff e Unit
modifyScope k s = do
  s' <- readScope s
  w <- unsafeInterleaveEff $ k s'
  extendScope w s

foreign import readScope
  "function readScope($scope){\
  \  return function(){\
  \    return $scope;\
  \  };\
  \}" :: forall e a. Scope a -> ReadEff e a

foreign import writeScopeFn
  "function writeScopeFn(prop, value, $scope){\
  \  return function(){\
  \    $scope[prop] = value;\
  \  };\
  \}" :: forall e a b. Fn3 String b (Scope a) (WriteEff e)

writeScope :: forall e a b. String -> b -> Scope a -> WriteEff e
writeScope = runFn3 writeScopeFn

foreign import extendScopeFn
  "function extendScopeFn(obj, $scope){\
  \  return function(){\
  \    angular.extend($scope, obj);\
  \  };\
  \}" :: forall e a b. Fn2 { | b } (Scope a) (WriteEff e)

extendScope :: forall e a b. { | b } -> Scope a -> WriteEff e
extendScope = runFn2 extendScopeFn

foreign import newScopeFn
  " function newScopeFn(isolate, $scope){ \
  \   return $scope.$new(isolate); \
  \ }"
  :: forall e a b. Fn2 Boolean (Scope a) (ScopeEff e (Scope b))

newScope :: forall e a b. Boolean -> Scope a -> ScopeEff e (Scope b)
newScope = runFn2 newScopeFn

foreign import watchFn
  " function watchFn(maybe, exp, listener, objEq, $scope){ \
  \   return maybe(function(){return $scope.$watch(exp, undefined, objEq);}) \
  \               (function(k){return function(){return $scope.$watch(exp, function(a1,a2,$scope){k(a1)(a2)($scope)()}, objEq);}}) \
  \               (listener); \
  \ }"
  :: forall e a b. Fn5 (Eff e WatchDeregistration
                       -> (WatchListener e a b -> Eff e WatchDeregistration)
                       -> Maybe (WatchListener e a b)
                       -> Eff e WatchDeregistration)
                       String
                       (Maybe (WatchListener e a b))
                       Boolean
                       (Scope b)
                       (ScopeEff e WatchDeregistration)

watch :: forall e a b. String -> Maybe (WatchListener e a b) -> Boolean -> Scope b -> ScopeEff e WatchDeregistration
watch = runFn5 watchFn maybe

foreign import watchCollectionFn
  " function watchCollectionFn(exp, listener, $scope){ \
  \   return function(){ \
  \     return $scope.$watchCollection(exp, function(a1,a2,$scope){ \
  \       return listener(a1)(a2)($scope)(); \
  \     }); \
  \   }; \
  \ }"
  :: forall e a b. Fn3 String (WatchListener e a b) (Scope b) (ScopeEff e WatchDeregistration)

watchCollection :: forall e a b. String -> WatchListener e a b -> Scope b -> ScopeEff e WatchDeregistration
watchCollection = runFn3 watchCollectionFn

foreign import digest
  " function digest($scope){ \
  \   return function(){ \
  \     return $scope.$digest(); \
  \   }; \
  \ }"
  :: forall e a. Scope a -> ScopeEff e Unit

foreign import destroy
  " function destroy($scope){ \
  \   return function(){ \
  \     return $scope.$destroy(); \
  \   }; \
  \ }"
  :: forall e a. Scope a -> ScopeEff e Unit

foreign import evalSyncFn
  " function evalSyncFn(maybe, expr, locals, $scope){ \
  \   return maybe(function(){return $scope.$eval(undefined, maybe(undefined)(angular.identity)(locals));}) \
  \               (function(k){return function(){return $scope.$eval(function($scope){return k($scope)();}, \
  \                                                                  maybe(undefined)(angular.identity)(locals));}}) \
  \               (expr); \
  \ } "
  :: forall e r s t a b c. Fn4 (t -> (s -> t) -> Maybe s -> t)
                               (Maybe (Scope a -> Eff e r))
                               (Maybe { | b })
                               (Scope a)
                               (ScopeEff e r)

evalSync :: forall e r a b. Maybe (Scope a -> Eff e r) -> Maybe { | b } -> Scope a -> ScopeEff e r
evalSync = runFn4 evalSyncFn maybe

foreign import evalAsyncFn
  " function evalAsyncFn(maybe, expr, scope){ \
  \   return maybe(function(){return scope.$evalAsync();}) \
  \               (function(k){return function(){return scope.$evalAsync(function(scope){return k(scope)();});}}) \
  \               (expr); \
  \ } "
  :: forall e r s t a b. Fn3 (t -> (s -> t) -> Maybe s -> t)
                             (Maybe (Scope a -> Eff e r))
                             (Scope a)
                             (ScopeEff e r)

evalAsync :: forall e r a. Maybe (Scope a -> Eff e r) -> Scope a -> ScopeEff e r
evalAsync = runFn3 evalAsyncFn maybe

defaultApplyExpr :: forall e r a. ApplyExpr e r a
defaultApplyExpr = DefaultApplyExpr

stringApplyExpr :: forall e r a. String -> ApplyExpr e r a
stringApplyExpr = StringApplyExpr

applyExpr :: forall e r a. (Scope a -> Eff e r) -> ApplyExpr e r a
applyExpr = FnApplyExpr

cataApply :: forall e r a b
          .  b
          -> (String -> b)
          -> ((Scope a -> Eff e r) -> b)
          -> ApplyExpr e r a
          -> b
cataApply f g h a =
  case a of
       DefaultApplyExpr -> f
       StringApplyExpr a -> g a
       FnApplyExpr a -> h a

foreign import applyFn
  " function applyFn(expr, $scope){ \
  \   return function(){ \
  \     return $scope.$apply(cataApply(undefined) \
  \                                   (angular.identity) \
  \                                   (function(a){return a(scope)();}) \
  \                                   (expr)); \
  \   }; \
  \ }"
  :: forall e r a. Fn2 (ApplyExpr e r a) (Scope a) (ScopeEff e r)

apply :: forall e r a. ApplyExpr e r a -> Scope a -> ScopeEff e r
apply = runFn2 applyFn

foreign import onFn
  " function onFn(name, listener, $scope){ \
  \   return function(){ \
  \     return $scope.$on(name, function(event, arg){ \
  \       return listener(event)(arg)(); \
  \     }); \
  \   }; \
  \ }"
  :: forall e a b c. Fn3 String
                         (Event e a b -> c -> Eff e Unit)
                         (Scope b)
                         (ScopeEff e OnDeregistration)

on :: forall e a b c. String -> (Event e a b -> c -> Eff e Unit) -> Scope b -> ScopeEff e OnDeregistration
on = runFn3 onFn

foreign import emitFn
  " function emitFn(name, arg, $scope){ \
  \   return function(){ \
  \     return $scope.$emit(name, arg); \
  \   }; \
  \ } "
  :: forall e a b c. Fn3 String a (Scope b) (ScopeEff e (Event e b c))

emit :: forall e a b c. String -> a -> Scope b -> ScopeEff e (Event e b c)
emit = runFn3 emitFn

foreign import broadcastFn
  " function broadcastFn(name, arg, $scope){ \
  \   return function(){ \
  \     return $scope.$broadcast(name, arg); \
  \   }; \
  \ } "
  :: forall e a b c. Fn3 String a (Scope b) (ScopeEff e (Event e b c))

broadcast :: forall e a b c. String -> a -> Scope b -> ScopeEff e (Event e b c)
broadcast = runFn3 broadcastFn

foreign import deregisterWatch "function deregisterWatch(dereg){return dereg();}" :: WatchDeregistration -> Unit

foreign import deregisterOn "function deregisterOn(dereg){return dereg();}" :: OnDeregistration -> Unit

foreign import id "function id(scope){return scope.$id;}" :: forall a. Scope a -> String

foreign import root "function root(scope){return scope.$root;}" :: forall a b. Scope a -> Scope b

foreign import parentFn
  " function parentFn(nothing, just, scope){ \
  \   var a = scope.$parent; \
  \   return a ? just(a) : nothing; \
  \ }"
  :: forall a b. Fn3 (Maybe (Scope b)) (Scope b -> Maybe (Scope b)) (Scope a) (Maybe (Scope b))

parent :: forall a b. Scope a -> Maybe (Scope b)
parent = runFn3 parentFn Nothing Just
