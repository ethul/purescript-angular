module Angular.NgModelController
 ( NgModelController()
 , NgModelCtrl()
 , Parser()
 , Formatter()
 , ValidationErrorKey()
 , render
 , setRender
 , isEmpty
 , setIsEmpty
 , setValidity
 , setPristine
 , setViewValue
 , viewValue
 , modelValue
 , setModelValue
 , appendParsers
 , prependParsers
 , appendFormatters
 , prependFormatters
 , appendViewChangeListeners
 , prependViewChangeListeners
 , error
 , pristine
 , dirty
 , valid
 , invalid
 ) where

import Control.Monad.Eff
import Data.Function
import Data.Maybe

foreign import data NgModelController :: * -> *

foreign import data NgModelCtrl :: !

type ValidationErrorKey = String

type Parser a = String -> Maybe a

type Formatter a = a -> String

foreign import render
  " function render($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$render(); \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Unit

foreign import setRender
  " function setRender(render){ \
  \   return function($ctrl){ \
  \     $ctrl.$render = render; \
  \     return {}; \
  \   }; \
  \ } "
  :: forall e a. Eff (ngmodel :: NgModelCtrl | e) Unit -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Unit

foreign import isEmpty
  " function isEmpty(value){ \
  \   return function($ctrl){ \
  \     return $ctrl.$isEmpty(value); \
  \   }; \
  \ } "
  :: forall e a. a -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Boolean

foreign import setIsEmpty
  " function setIsEmpty(isEmpty){ \
  \   return function($ctrl){ \
  \     $ctrl.$isEmpty = function(a){return isEmpty(a)();}; \
  \     return $ctrl; \
  \   }; \
  \ } "
  :: forall e a b. (b -> Eff (ngmodel :: NgModelCtrl | e) Boolean) -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) (NgModelController b)

foreign import setValidity
  " function setValidity(validationErrorKey){ \
  \   return function(isValid){ \
  \     return function($ctrl){ \
  \       return $ctrl.$setValidity(validationErrorKey, isValid); \
  \     }; \
  \   }; \
  \ } "
  :: forall e a. ValidationErrorKey -> Boolean -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Unit

foreign import setPristine
  " function setPristine($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$setPristine(); \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Unit

foreign import setViewValue
  " function setViewValue(value){ \
  \   return function($ctrl){ \
  \     return function(){ \
  \       return $ctrl.$setViewValue(value); \
  \     }; \
  \   }; \
  \ } "
  :: forall e a. String -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Unit

foreign import viewValue
  " function viewValue($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$viewValue; \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) String

foreign import modelValue
  " function modelValue($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$modelValue; \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) a

foreign import setModelValue
  " function setModelValue(value){ \
  \   return function($ctrl){ \
  \     return function(){ \
  \       $ctrl.$modelValue = value; \
  \       return $ctrl; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a b. b -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) (NgModelController b)

foreign import appendParsersFn
  " function appendParsersFn(fromMaybe, parsers, $ctrl){ \
  \   return function(){ \
  \     var as = []; \
  \     angular.forEach(parsers, function(p){ \
  \       as.push(function(v){ \
  \         return fromMaybe(undefined)(p(v)); \
  \       }); \
  \     }); \
  \     $ctrl.$parsers.push.apply($ctrl.$parsers, as); \
  \     return {}; \
  \   }; \
  \ } "
  :: forall e a. Fn3 (a -> Maybe a -> a)
                     [Parser a]
                     (NgModelController a)
                     (Eff (ngmodel :: NgModelCtrl | e) Unit)

appendParsers :: forall e a. [Parser a] -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Unit
appendParsers = runFn3 appendParsersFn fromMaybe

foreign import prependParsersFn
  " function prependParsersFn(fromMaybe, parsers, $ctrl){ \
  \   return function(){ \
  \     var as = []; \
  \     angular.forEach(parsers, function(p){ \
  \       as.push(function(v){ \
  \         return fromMaybe(undefined)(p(v)); \
  \       }); \
  \     }); \
  \     $ctrl.$parsers.unshift.apply($ctrl.$parsers, as); \
  \     return {}; \
  \   }; \
  \ } "
  :: forall e a. Fn3 (a -> Maybe a -> a)
                     [Parser a]
                     (NgModelController a)
                     (Eff (ngmodel :: NgModelCtrl | e) Unit)

prependParsers :: forall e a. [Parser a] -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Unit
prependParsers = runFn3 prependParsersFn fromMaybe

foreign import appendFormatters
  " function appendFormatters(formatters){ \
  \   return function($ctrl){ \
  \     return function(){ \
  \       $ctrl.$formatters.push.apply($ctrl.$formatters, formatters); \
  \       return {}; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a. [Formatter a] -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Unit

foreign import prependFormatters
  " function prependFormatters(formatters){ \
  \   return function($ctrl){ \
  \     return function(){ \
  \       $ctrl.$formatters.unshift.apply($ctrl.$formatters, formatters); \
  \       return {}; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a. [Formatter a] -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Unit

foreign import appendViewChangeListeners
  " function appendViewChangeListeners(listeners){ \
  \   return function($ctrl){ \
  \     return function(){ \
  \       $ctrl.$viewChangeListeners.push.apply($ctrl.$viewChangeListeners, listeners); \
  \       return {}; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a. [Eff e Unit] -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Unit

foreign import prependViewChangeListeners
  " function prependViewChangeListeners(listeners){ \
  \   return function($ctrl){ \
  \     return function(){ \
  \       $ctrl.$viewChangeListeners.unshift.apply($ctrl.$viewChangeListeners, listeners); \
  \       return {}; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a. [Eff e Unit] -> NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Unit

foreign import error
  " function error($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$error; \
  \   }; \
  \ } "
  :: forall e a b. NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) { | b }

foreign import pristine
  " function pristine($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$pristine; \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Boolean

foreign import dirty
  " function dirty($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$dirty; \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Boolean

foreign import valid
  " function valid($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$valid; \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Boolean

foreign import invalid
  " function invalid($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$invalid; \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> Eff (ngmodel :: NgModelCtrl | e) Boolean
