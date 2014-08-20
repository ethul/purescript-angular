module Angular.Q
  ( Q()
  , QEff()
  , NgQ()
  , defer
  , reject
  , when
  , all
  ) where

import Control.Monad.Eff
import Data.Function

import Angular.Deferred (Deferred())
import Angular.Promise (Promise())

foreign import data Q :: *

foreign import data NgQ :: !

type QEff e r = Eff (ngq :: NgQ | e) r

foreign import defer
  " function defer($q){ \
  \   return function(){ \
  \     return $q.defer(); \
  \   }; \
  \ } "
  :: forall e a b. Q -> (QEff e (Deferred a b))

foreign import rejectFn
  " function rejectFn(e, $q){ \
  \   return function(){ \
  \     return $q.reject(e); \
  \   }; \
  \ } "
  :: forall e a b. Fn2 a Q (QEff e (Promise a b))

reject :: forall e a b. a -> Q -> QEff e (Promise a b)
reject = runFn2 rejectFn

foreign import whenFn
  " function whenFn(a, $q){ \
  \   return function(){ \
  \     return $q.when(a); \
  \   }; \
  \ } "
  :: forall e a b. Fn2 b Q (QEff e (Promise a b))

when :: forall e a b. b -> Q -> QEff e (Promise a b)
when = runFn2 whenFn

foreign import allFn
  " function allFn(a, $q){ \
  \   return function(){ \
  \     return $q.all(a); \
  \   }; \
  \ } "
  :: forall e a b. Fn2 [Promise a b] Q (QEff e (Promise b [a]))

all :: forall e a b. [Promise a b] -> Q -> QEff e (Promise b [a])
all = runFn2 allFn
