module Angular.FormController
 ( FormController()
 , FormEff()
 , NgForm()
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

foreign import data NgForm :: !

type FormEff e r = Eff (ngform :: NgForm | e) r

foreign import addControl
  " function addControl(control){ \
  \   return function($ctrl){ \
  \     return function(){ \
  \       return $ctrl.$addControl(control); \
  \     }; \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> FormController -> FormEff e Unit

foreign import removeControl
  " function removeControl(control){ \
  \   return function($ctrl){ \
  \     return function(){ \
  \       return $ctrl.$removeControl(control); \
  \     }; \
  \   }; \
  \ } "
  :: forall e a. NgModelController a -> FormController -> FormEff e Unit

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
 :: forall e a. ValidationErrorKey -> Boolean -> NgModelController a -> FormController -> FormEff e Unit

foreign import setDirty
  " function setDirty($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$setDirty(); \
  \   }; \
  \ } "
  :: forall e. FormController -> FormEff e Unit

foreign import setPristine
  " function setPristine($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$setPristine(); \
  \   }; \
  \ } "
  :: forall e. FormController -> FormEff e Unit

foreign import pristine
  " function pristine($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$pristine; \
  \   }; \
  \ } "
  :: forall e. FormController -> FormEff e Boolean

foreign import dirty
  " function dirty($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$dirty; \
  \   }; \
  \ } "
  :: forall e. FormController -> FormEff e Boolean

foreign import valid
  " function valid($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$valid; \
  \   }; \
  \ } "
  :: forall e. FormController -> FormEff e Boolean

foreign import invalid
  " function invalid($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$invalid; \
  \   }; \
  \ } "
  :: forall e. FormController -> FormEff e Boolean

foreign import error
  " function error($ctrl){ \
  \   return function(){ \
  \     return $ctrl.$error; \
  \   }; \
  \ } "
  :: forall e a. FormController -> FormEff e { | a }
