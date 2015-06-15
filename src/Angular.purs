module Angular where

import Control.Monad.Eff

import Angular.Injector (Injector())
import Angular.Element (Element())

-- | bind

foreign import bootstrap
  " function bootstrap(element) { \
  \   return function(modules){ \
  \     return function(){ \
  \       return angular.bootstrap(element, modules); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. Element -> [String] -> Eff e Injector

foreign import copy
  " function copy(src) { \
  \   return function() { \
  \     return angular.copy(src); \
  \   }; \
  \ }" :: forall e a. a -> Eff e a

-- | element => Angular.Element

-- | equals

foreign import extend
  " function extend(dst) { \
  \   return function(src) { \
  \     return function() { \
  \       return angular.extend(dst, src); \
  \     }; \
  \   }; \
  \ }" :: forall e a b c. { | a } -> { | b } -> Eff e { | c }

-- | forEach

-- | fromJson

-- | identity

-- | injector, see Angular.Injector

foreign import isArray
  " function isArray(value) { \
  \   return function() { \
  \     return angular.isArray(value); \
  \   }; \
  \ }" :: forall e a. a -> Eff e Boolean

foreign import isDate
  " function isDate(value) { \
  \   return function() { \
  \     return angular.isDate(value); \
  \   }; \
  \ }" :: forall e a. a -> Eff e Boolean

foreign import isDefined
  " function isDefined(value) { \
  \   return function() { \
  \     return angular.isDefined(value); \
  \   }; \
  \ }" :: forall e a. a -> Eff e Boolean

foreign import isElement
  " function isElement(value) { \
  \   return function() { \
  \     return angular.isElement(value); \
  \   }; \
  \ }" :: forall e a. a -> Eff e Boolean

foreign import isFunction
  " function isFunction(value) { \
  \   return function() { \
  \     return angular.isFunction(value); \
  \   }; \
  \ }" :: forall e a. a -> Eff e Boolean

foreign import isNumber
  " function isNumber(value) { \
  \   return function() { \
  \     return angular.isNumber(value); \
  \   }; \
  \ }" :: forall e a. a -> Eff e Boolean

foreign import isObject
  " function isObject(value) { \
  \   return function() { \
  \     return angular.isObject(value); \
  \   }; \
  \ }" :: forall e a. a -> Eff e Boolean

foreign import isString
  " function isString(value) { \
  \   return function() { \
  \     return angular.isString(value); \
  \   }; \
  \ }" :: forall e a. a -> Eff e Boolean

foreign import isUndefined
  " function isUndefined(value) { \
  \   return function() { \
  \     return angular.isUndefined(value); \
  \   }; \
  \ }" :: forall e a. a -> Eff e Boolean

foreign import lowercase
  " function lowercase(str) { \
  \   return function() { \
  \     return angular.lowercase(str); \
  \   }; \
  \ }" :: forall e. String -> Eff e String

-- | module => Angular.Module

-- | noop

-- | toJson

-- | uppercase
