module Angular.DI (
    Dependency, name
  , Service
  , RootScope(..)
  , RootElement(..)
  , get
  , Injectable, dependencies
  , Annotated()
  , annotate
  ) where

import Control.Monad.Eff (Eff())
import Data.Function
import Angular.Injector (Injector(), InjEff())
import Angular.Animate (Animate())
import Angular.Cache (CacheFactory())
import Angular.Http (Http())
import Angular.Interpolate (Interpolate())
import Angular.Interval (Interval())
import Angular.Location (Location())
import Angular.Log (Log())
import Angular.Parse (Parse())
import Angular.Q (Q())
import Angular.Timeout (Timeout())
import Angular.Scope (Scope())
import Angular.Element (Element())
import Angular.Attributes (Attributes())
import Angular.This (This())

-- | A type which can by provided by dependency injection.
class Dependency a where
  name :: String

-- | Services which can be retrieved with get
class (Dependency a) <= Service a

instance dependencyInjector :: Dependency Injector where
  name = "$injector"
instance serviceInjector :: Service Injector

instance dependencyAnimate :: Dependency Animate where
  name = "$animate"
instance serviceAnimate :: Service Animate

instance dependencyCacheFactory :: Dependency CacheFactory where
  name = "$cacheFactory"
instance serviceCacheFactory :: Service CacheFactory

instance dependencyHttp :: Dependency Http where
  name = "$http"
instance serviceHttp :: Service Http

instance dependencyInterpolate :: Dependency Interpolate where
  name = "$interpolate"
instance serviceInterpolate :: Service Interpolate

instance dependencyInterval :: Dependency Interval where
  name = "$interval"
instance serviceInterval :: Service Interval

instance dependencyLocation :: Dependency Location where
  name = "$location"
instance serviceLocation :: Service Location

instance dependencyLog :: Dependency Log where
  name = "$log"
instance serviceLog :: Service Log

instance dependencyParse :: Dependency Parse where
  name = "$parse"
instance serviceParse :: Service Parse

instance dependencyQ :: Dependency Q where
  name = "$q"
instance serviceQ :: Service Q

instance dependencyTimeout :: Dependency Timeout where
  name = "$timeout"
instance serviceTimeout :: Service Timeout

newtype RootScope a = RootScope (Scope a)

instance dependencyRootScope :: Dependency (RootScope a) where
  name = "$rootScope"
instance serviceRootScope :: Service (RootScope a)

instance dependencyScope :: Dependency (Scope a) where
  name = "$scope"

newtype RootElement = RootElement Element

instance dependencyRootElement :: Dependency RootElement where
  name = "$rootElement"
instance serviceRootElement :: Service RootElement

instance dependencyElement :: Dependency Element where
  name = "$element"

instance dependencyAttributes :: Dependency Attributes where
  name = "$attrs"

instance dependencyThis :: Dependency (This a) where
  -- it would be nice to make a dummy service to avoid the special handling
  -- in annotate, but there is nowhere to do so
  name = "$this"


foreign import getDependency
  "function get(dependency) {\
  \ dependency = dependency.name;\
  \ return function ($injector) {\
  \   return function () {\
  \     return $injector.get(dependency);\
  \   };\
  \ };\
  \}" :: forall e a . (Dependency a) => Injector -> InjEff e a

get :: forall e a . (Service a) => Injector -> InjEff e a
get = getDependency


class Injectable a where
  dependencies :: a -> [String]

instance injectableEff :: Injectable (Eff e r) where
  dependencies _ = []

foreign import dependenciesFn
  "function dependenciesFn(dependency) {\
  \ dependency = [dependency.name];\
  \ return function (injectable) {\
  \   return function (/*f*/) {\
  \     return dependency.concat(injectable.dependencies(/*f(a)*/));\
  \   };\
  \ };\
  \}" :: forall a b . (Dependency a, Injectable b) => (a -> b) -> [String]

instance injectableFn :: (Dependency a, Injectable b) => Injectable (a -> b) where
  dependencies f = dependenciesFn f


-- | An Eff function with added dependency injection annotations.
foreign import data Annotated :: * -> *

-- | Infer and annotate a function with its dependencies, that can provided by Injector.invoke or Module functions.
foreign import annotate
  "function annotate(injectable) {\
  \ return function (fun) {\
  \   var inject = injectable.dependencies(fun);\
  \   function g(/*...*/) {\
  \     var f = fun;\
  \     for (var i = 0, a = 0; i < inject.length; i++)\
  \       f = f(inject[i] === '$this' ? this : arguments[a++]);\
  \     return f();\
  \   }\
  \   g.prototype = fun.prototype;\
  \   g.$inject = inject.filter(function (d) { return d !== '$this'; });\
  \   return g;\
  \ };\
  \}" :: forall a . (Injectable a) => a -> Annotated a
