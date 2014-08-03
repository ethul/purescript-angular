module Angular.Injector
  ( Injector()
  , Inj()
  , InjEff()
  , get
  , invoke
  , has
  , instantiate
  , annotate
  , injector
  ) where

import Control.Monad.Eff
import Data.Function
import Data.Maybe

foreign import data Injector :: *

foreign import data Inj :: !

type InjEff e a = Eff (nginj :: Inj | e) a

get :: forall e a. String -> Injector -> InjEff e a
get = runFn2 getFn

invoke :: forall e r a b c. a -> Maybe { | b } -> Maybe { | c } -> Injector -> InjEff e r
invoke = runFn5 invokeFn maybe

has :: forall e. String -> Injector -> InjEff e Boolean
has = runFn2 hasFn

instantiate :: forall e r a b. a -> Maybe { | b } -> Injector -> InjEff e r
instantiate = runFn4 instantiateFn maybe

annotate :: forall e a. a -> Injector -> InjEff e [String]
annotate = runFn2 annotateFn

foreign import injector
  " function injector(modules){ \
  \   return function(){ \
  \     return angular.injector(modules); \
  \   }; \
  \ } "
  :: forall e. [String] -> InjEff e Injector

foreign import getFn
  " function getFn(name, $injector){ \
  \   return function(){ \
  \     return $injector.get(name); \
  \   } \
  \ } "
  :: forall e a. Fn2 String Injector (InjEff e a)

foreign import invokeFn
  " function invokeFn(maybe, fn, self, locals, $injector){ \
  \   return function(){ \
  \     return $injector.invoke(fn, \
  \                             maybe(undefined)(angular.identity)(self), \
  \                             maybe(undefined)(angular.identity)(locals)); \
  \   } \
  \ } "
  :: forall e r s t u a b c. Fn5 (t -> (u -> t) -> Maybe s -> t) a (Maybe { | b }) (Maybe { | c }) Injector (InjEff e r)

foreign import hasFn
  " function hasFn(name, $injector){ \
  \   return function(){ \
  \     return $injector.get(name); \
  \   } \
  \ } "
  :: forall e. Fn2 String Injector (InjEff e Boolean)

foreign import instantiateFn
  " function instantiateFn(maybe, type, locals, $injector){ \
  \   return function(){ \
  \     return $injector.instantiate(type, maybe(undefined)(angular.identity)(locals)); \
  \   } \
  \ } "
  :: forall e r s t u a b. Fn4 (t -> (u -> t) -> Maybe s -> t) a (Maybe { | b }) Injector (InjEff e r)

foreign import annotateFn
  " function annotateFn(fn, $injector){ \
  \   return function(){ \
  \     return $injector.annotate(fn); \
  \   } \
  \ } "
  :: forall e a. Fn2 a Injector (InjEff e [String])
