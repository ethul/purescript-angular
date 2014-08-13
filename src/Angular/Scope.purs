module Angular.Scope
  ( Scope()
  , ReadScope()
  , WriteScope()
  , Read()
  , Write()
  , ReadWrite()
  , WatchListener()
  , Event()
  , ApplyExpr()
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
  = FnApplyExpr (Scope a -> Eff e r)
  | StringApplyExpr String
  | DefaultApplyExpr

foreign import data Scope :: # * -> *

foreign import data WatchDeregistration :: *

foreign import data OnDeregistration :: *

foreign import data ReadScope :: !

foreign import data WriteScope :: !

type Read e a = Eff (ngrscope :: ReadScope | e) { | a }

type Write e = Eff (ngwscope :: WriteScope | e) Unit

type ReadWrite e r = Eff (ngrscope :: ReadScope, ngwscope :: WriteScope | e) r

writeScope :: forall e a b. String -> b -> Scope a -> Write e
writeScope = runFn3 writeScopeFn

extendScope :: forall e a b. { | b } -> Scope a -> Write e
extendScope = runFn2 extendScopeFn

modifyScope :: forall e f a b. ({ | a} -> Eff f { | b }) -> Scope a -> ReadWrite e Unit
modifyScope k s = do
  s' <- readScope s
  w <- unsafeInterleaveEff $ k s'
  extendScope w s

foreign import readScope
  "function readScope(scope){\
  \  return function(){\
  \    return scope;\
  \  };\
  \}" :: forall e a. Scope a -> Read e a

foreign import writeScopeFn
  "function writeScopeFn(prop, value, scope){\
  \  return function(){\
  \    scope[prop] = value;\
  \  };\
  \}" :: forall e a b. Fn3 String b (Scope a) (Write e)

foreign import extendScopeFn
  "function extendScopeFn(obj, scope){\
  \  return function(){\
  \    angular.extend(scope, obj);\
  \  };\
  \}" :: forall e a b. Fn2 { | b } (Scope a) (Write e)

foreign import newScope
  " function newScope(isolate){ \
  \   return function(scope){ \
  \     return scope.$new(isolate); \
  \   }; \
  \ }"
  :: forall a b. Boolean -> Scope a -> Scope b

foreign import watchFn
  " function watchFn(maybe, exp, listener, objEq, scope){ \
  \   return maybe(function(){return scope.$watch(exp, undefined, objEq);}) \
  \               (function(k){return function(){return scope.$watch(exp, function(a1,a2,scope){k(a1)(a2)(scope)()}, objEq);}}) \
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
                       (Eff e WatchDeregistration)

watch :: forall e a b. String -> Maybe (WatchListener e a b) -> Boolean -> Scope b -> Eff e WatchDeregistration
watch = runFn5 watchFn maybe

foreign import watchCollection
  " function watchCollection(exp){ \
  \   return function(listener){ \
  \     return function(scope){ \
  \       return function(){ \
  \         return scope.$watchCollection(exp, function(a1,a2,scope){ \
  \           return listener(a1)(a2)(scope)(); \
  \         }); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e a b
  .  String
  -> WatchListener e a b
  -> Scope b
  -> Eff e WatchDeregistration

foreign import digest
  " function digest(scope){ \
  \   return function(){ \
  \     return scope.$digest(); \
  \   }; \
  \ }"
  :: forall e a. Scope a -> Eff e Unit

foreign import destroy
  " function destroy(scope){ \
  \   return function(){ \
  \     return scope.$destroy(); \
  \   }; \
  \ }"
  :: forall e a. Scope a -> Eff e Unit

foreign import evalSyncFn
  " function evalSyncFn(maybe, expr, locals, scope){ \
  \   return maybe(function(){return scope.$eval(undefined, maybe(undefined)(angular.identity)(locals));}) \
  \               (function(k){return function(){return scope.$eval(function(scope){return k(scope)();}, \
  \                                                                 maybe(undefined)(angular.identity)(locals));}}) \
  \               (expr); \
  \ } "
  :: forall e r s t a b c. Fn4 (t -> (s -> t) -> Maybe s -> t)
                               (Maybe (Scope a -> Eff e r))
                               (Maybe { | b })
                               (Scope a)
                               (Eff e r)

evalSync :: forall e r a b. Maybe (Scope a -> Eff e r) -> Maybe { | b } -> Scope a -> Eff e r
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
                             (Eff e r)

evalAsync :: forall e r a. Maybe (Scope a -> Eff e r) -> Scope a -> Eff e r
evalAsync = runFn3 evalAsyncFn maybe

applyExpr :: forall e r a. (Scope a -> Eff e r) -> ApplyExpr e r a
applyExpr = FnApplyExpr

stringApplyExpr :: forall e r a. String -> ApplyExpr e r a
stringApplyExpr = StringApplyExpr

defaultApplyExpr :: forall e r a. ApplyExpr e r a
defaultApplyExpr = DefaultApplyExpr

foreign import apply
  " function __apply_expr__(k){ \
  \   if (k.ctor === FnApplyExpr().ctor) return function(scope){return k.values[0](scope)();}; \
  \   else if (k.ctor === StringApplyExpr().ctor) return k.values[0]; \
  \   else if (k.ctor === DefaultApplyExpr.ctor) return undefined; \
  \   else throw 'Failed pattern match'; \
  \ } \
  \ function apply(expr){ \
  \   return function(scope){ \
  \     return function(){ \
  \       return scope.$apply(__apply_expr__(expr)); \
  \     }; \
  \   }; \
  \ }"
  :: forall e r a
  .  ApplyExpr e r a
  -> Scope a
  -> Eff e r

foreign import on
  " function on(name){ \
  \   return function(listener){ \
  \     return function(scope){ \
  \       return function(){ \
  \         return scope.$on(name, function(event, arg){ \
  \           return listener(event)(arg)(); \
  \         }); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e a b c
  .  String
  -> (Event e a b -> c -> Eff e Unit)
  -> Scope b
  -> Eff e OnDeregistration

foreign import emit
  "function emit(name){\
  \  return function(arg){\
  \    return function(scope){\
  \      return function(){\
  \        return scope.$emit(name, arg);\
  \      };\
  \    };\
  \  };\
  \}"
  :: forall e a b c
  .  String
  -> a
  -> Scope b
  -> Eff e (Event e b c)

foreign import broadcast
  "function broadcast(name){\
  \  return function(arg){\
  \    return function(scope){\
  \      return function(){\
  \        return scope.$broadcast(name, arg);\
  \      };\
  \    };\
  \  };\
  \}"
  :: forall e a b c
  .  String
  -> a
  -> Scope b
  -> Eff e (Event e b c)

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
