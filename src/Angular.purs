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

-- | isArray

-- | isDate

-- | isDefined

-- | isElement

-- | isFunction

-- | isNumber

-- | isObject

-- | isString

-- | isUndefined

-- | lowercase

-- | module => Angular.Module

-- | noop

-- | toJson

-- | uppercase
