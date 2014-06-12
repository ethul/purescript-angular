module Angular.Scope where

import Prelude (Unit(..))

import Control.Monad.Eff
import Data.Maybe

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

foreign import newScope
  " function newScope(isolate){ \
  \   return function(scope){ \
  \     return scope.$new(isolate); \
  \   }; \
  \ }"
  :: forall a b. Boolean -> Scope a -> Scope b

foreign import watch
  " function watch(exp){ \
  \   return function(listener){ \
  \     return function(objEq){ \
  \       return function(scope){ \
  \         return function(){ \
  \           return ( \
  \             listener.ctor === Data_Maybe.Nothing.ctor ? \
  \               scope.$watch(exp, undefined, objEq)     : \
  \               scope.$watch(exp, function(a1,a2,scope){listener.values[0](a1)(a2)(scope)()}, objEq) \
  \           ); \
  \         }; \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e a b
  .  String
  -> Maybe (WatchListener e a b)
  -> Boolean
  -> Scope b
  -> Eff e WatchDeregistration

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

foreign import evalSync
  " function evalSync(expr){ \
  \   return function(locals){ \
  \     return function(scope){ \
  \       return function(){ \
  \         return ( \
  \           expr.ctor === Data_Maybe.Nothing.ctor      ? \
  \             scope.$eval(undefined, locals.values[0]) : \
  \             scope.$eval(function(scope){return expr(scope)();}, locals.values[0]) \
  \         ); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e r a b c
  .  Maybe (Scope a -> Eff e r)
  -> Maybe { | b }
  -> Scope a
  -> Eff e r

foreign import evalAsync
  " function evalAsync(expr){ \
  \   return function(scope){ \
  \     return function(){ \
  \       return ( \
  \         expr.ctor === Data_Maybe.Nothing.ctor ? \
  \           scope.$evalAsync()                  : \
  \           scope.$evalAsync(function(scope){return expr(scope)();}) \
  \       ); \
  \     }; \
  \   }; \
  \ }"
  :: forall e r a
  .  Maybe (Scope a -> Eff e r)
  -> Scope a
  -> Eff e Unit

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

foreign import parent
  " function parent(scope){ \
  \   var a = scope.$parent; \
  \   return a ? Data_Maybe.Just(a) : Data_Maybe.Nothing; \
  \ }"
  :: forall a b. Scope a -> Maybe (Scope b)

-- | TODO: Fix
howCanWeEnsureDataMaybeIsIncluded = Nothing
