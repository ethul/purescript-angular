module Angular.Module
  ( Module()
  , ReadModule()
  , WriteModule()
  , RegisterToModule()
  , Read()
  , Write()
  , Register()
  , ngmodule
  , ngmodule'
  , provider
  , factory
  , service
  , value
  , constant
  , animation
  , filter
  , controller
  , directive
  , config
  , run
  ) where

import Control.Monad.Eff
import Data.Function

foreign import data Module :: *

foreign import data ReadModule :: !

foreign import data WriteModule :: !

foreign import data RegisterToModule :: !

type Read e = Eff (ngrmod :: ReadModule | e) Module

type Write e = Eff (ngwmod :: WriteModule | e) Module

type Register e = Eff (nggmod :: RegisterToModule | e) Module

ngmodule :: forall e. String -> Read e
ngmodule = readModuleFn

ngmodule' :: forall e. String -> [String] -> Write e
ngmodule' = runFn2 writeModuleFn

provider :: forall e a. String -> a -> Module -> Register e
provider = runFn4 registerNamedToModuleFn "provider"

factory :: forall e a. String -> a -> Module -> Register e
factory = runFn4 registerNamedToModuleFn "factory"

service :: forall e a. String -> a -> Module -> Register e
service = runFn4 registerNamedToModuleFn "service"

value :: forall e a. String -> a -> Module -> Register e
value = runFn4 registerNamedToModuleFn "value"

constant :: forall e a. String -> a -> Module -> Register e
constant = runFn4 registerNamedToModuleFn "constant"

animation :: forall e a. String -> a -> Module -> Register e
animation = runFn4 registerNamedToModuleFn "animation"

filter :: forall e a. String -> a -> Module -> Register e
filter = runFn4 registerNamedToModuleFn "filter"

controller :: forall e a. String -> a -> Module -> Register e
controller = runFn4 registerNamedToModuleFn "controller"

directive :: forall e a. String -> a -> Module -> Register e
directive = runFn4 registerNamedToModuleFn "directive"

config :: forall e a. a -> Module -> Register e
config = runFn3 registerUnnamedToModuleFn "config"

run :: forall e a. a -> Module -> Register e
run = runFn3 registerUnnamedToModuleFn "run"

foreign import readModuleFn
  "function readModuleFn(name){\
  \  return function(){\
  \    return angular.module(name);\
  \  };\
  \}" :: forall e. String -> Read e

foreign import writeModuleFn
  "function writeModuleFn(name, modules){\
  \  return function(){\
  \    return angular.module(name, modules);\
  \  };\
  \}" :: forall e. Fn2 String [String] (Write e)

foreign import registerNamedToModuleFn
  "function registerNamedToModuleFn(type, name, a, module){\
  \  return function(){\
  \    return module[type](name, a);\
  \  };\
  \}" :: forall e a. Fn4 String String a Module (Register e)

foreign import registerUnnamedToModuleFn
  "function registerUnnamedToModuleFn(type, a, module){\
  \  return function(){\
  \    return module[type](a);\
  \  };\
  \}" :: forall e a. Fn3 String a Module (Register e)
