module Angular.FormController
 ( FormController()
 , FormCtrl()
 , addControl
 , removeControl
 , setValidity
 , setDirty
 , setPristine
 , pristine
 , dirty
 , valid
 , invalid
 , error
 ) where

import Control.Monad.Eff

import Angular.NgModelController (NgModelController(), ValidationErrorKey())

foreign import data FormController :: *

foreign import data FormCtrl :: !

foreign import addControl
  " function addControl(control){ \
  \   return function($ctrl){ \
  \     return function(){ \
  \       return $ctrl.$addControl(control); \
  \     }; \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> FormController -> Eff (ngform :: FormCtrl | e) Unit

foreign import removeControl
  " function removeControl(control){ \
  \   return function($ctrl){ \
  \     return function(){ \
  \       return $ctrl.$removeControl(control); \
  \     }; \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> FormController -> Eff (ngform :: FormCtrl | e) Unit

foreign import setValidity
  " function setValidity(key){ \
  \   return function(isValid){ \
  \     return function(control){ \
  \       return function($ctrl){ \
  \         return function(){ \
  \           return $ctrl.$setValidity(key, isValid, control, $ctrl); \
  \         }; \
  \       }; \
  \     }; \
  \   }; \
  \ } "
 :: forall e a. ValidationErrorKey -> Boolean -> NgModelController a -> FormController -> Eff (ngform :: FormCtrl | e) Unit

foreign import setDirty
  " function setDirty($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$setDirty(); \
  \   }; \
  \ } "
  :: forall e. FormController -> Eff (ngform :: FormCtrl | e) Unit

foreign import setPristine
  " function setPristine($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$setPristine(); \
  \   }; \
  \ } "
  :: forall e. FormController -> Eff (ngform :: FormCtrl | e) Unit

foreign import pristine
  " function pristine($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$pristine; \
  \   }; \
  \ } "
  :: forall e. FormController -> Eff (ngform :: FormCtrl | e) Boolean

foreign import dirty
  " function dirty($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$dirty; \
  \   }; \
  \ } "
  :: forall e. FormController -> Eff (ngform :: FormCtrl | e) Boolean

foreign import valid
  " function valid($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$valid; \
  \   }; \
  \ } "
  :: forall e. FormController -> Eff (ngform :: FormCtrl | e) Boolean

foreign import invalid
  " function invalid($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$invalid; \
  \   }; \
  \ } "
  :: forall e. FormController -> Eff (ngform :: FormCtrl | e) Boolean

foreign import error
  " function error($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$error; \
  \   }; \
  \ } "
  :: forall e a. FormController -> Eff (ngform :: FormCtrl | e) { | a }
