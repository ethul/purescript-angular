module Angular.Module
  ( Module()
  , NgReadModule()
  , NgWriteModule()
  , NgRegisterToModule()
  , ReadEff()
  , WriteEff()
  , RegisterEff()
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

foreign import data NgReadModule :: !

foreign import data NgWriteModule :: !

foreign import data NgRegisterToModule :: !

type ReadEff e = Eff (ngrmod :: NgReadModule | e) Module

type WriteEff e = Eff (ngwmod :: NgWriteModule | e) Module

type RegisterEff e = Eff (nggmod :: NgRegisterToModule | e) Module

ngmodule :: forall e. String -> ReadEff e
ngmodule = readModuleFn

ngmodule' :: forall e. String -> [String] -> WriteEff e
ngmodule' = runFn2 writeModuleFn

provider :: forall e a. String -> a -> Module -> RegisterEff e
provider = runFn4 registerNamedToModuleFn "provider"

factory :: forall e a. String -> a -> Module -> RegisterEff e
factory = runFn4 registerNamedToModuleFn "factory"

service :: forall e a. String -> a -> Module -> RegisterEff e
service = runFn4 registerNamedToModuleFn "service"

value :: forall e a. String -> a -> Module -> RegisterEff e
value = runFn4 registerNamedToModuleFn "value"

constant :: forall e a. String -> a -> Module -> RegisterEff e
constant = runFn4 registerNamedToModuleFn "constant"

animation :: forall e a. String -> a -> Module -> RegisterEff e
animation = runFn4 registerNamedToModuleFn "animation"

filter :: forall e a. String -> a -> Module -> RegisterEff e
filter = runFn4 registerNamedToModuleFn "filter"

controller :: forall e a. String -> a -> Module -> RegisterEff e
controller = runFn4 registerNamedToModuleFn "controller"

directive :: forall e a. String -> a -> Module -> RegisterEff e
directive = runFn4 registerNamedToModuleFn "directive"

config :: forall e a. a -> Module -> RegisterEff e
config = runFn3 registerUnnamedToModuleFn "config"

run :: forall e a. a -> Module -> RegisterEff e
run = runFn3 registerUnnamedToModuleFn "run"

foreign import readModuleFn
  "function readModuleFn(name){\
  \  return function(){\
  \    return angular.module(name);\
  \  };\
  \}" :: forall e. String -> ReadEff e

foreign import writeModuleFn
  "function writeModuleFn(name, modules){\
  \  return function(){\
  \    return angular.module(name, modules);\
  \  };\
  \}" :: forall e. Fn2 String [String] (WriteEff e)

foreign import registerNamedToModuleFn
  "function registerNamedToModuleFn(type, name, a, module){\
  \  return function(){\
  \    return module[type](name, a);\
  \  };\
  \}" :: forall e a. Fn4 String String a Module (RegisterEff e)

foreign import registerUnnamedToModuleFn
  "function registerUnnamedToModuleFn(type, a, module){\
  \  return function(){\
  \    return module[type](a);\
  \  };\
  \}" :: forall e a. Fn3 String a Module (RegisterEff e)
