module Angular.Controller
  ( ConstructorFnEff(..)
  , This(..)
  , ScopeState(..)
  , ThisState(..)
  , ReadScopeState(..)
  , WriteScopeState(..)
  , ReadThisState(..)
  , WriteThisState(..)
  , RWScopeState(..)
  , RWThisState(..)
  , readScopeState
  , writeScopeState
  , modifyScopeState
  , readThisState
  , writeThisState
  , modifyThisState
  , controller
  ) where

import Control.Monad.Eff

import Angular.Injector (InjectDependency(..))
import Angular.Module (Module(..), WriteModule(..))
import Angular.Scope (Scope(..))

type ScopeState a = { | a }

type ThisState a = { | a }

type RWScopeState e a r
  = Eff ( ngrscope :: ReadScopeState a
        , ngwscope :: WriteScopeState a | e) r

type RWThisState e a r
  = Eff ( ngrthis :: ReadThisState a
        , ngwthis :: WriteThisState a | e) r

type ConstructorFnEff e r a b c d
  = Eff ( ngrscope :: ReadScopeState a
        , ngrthis :: ReadThisState b
        , ngwscope :: WriteScopeState c
        , ngwthis :: WriteThisState d
        , nginj :: InjectDependency | e ) r

foreign import data This :: # * -> *

foreign import data ReadScopeState :: # * -> !

foreign import data WriteScopeState :: # * -> !

foreign import data ReadThisState :: # * -> !

foreign import data WriteThisState :: # * -> !

foreign import readScopeState
  " function readScopeState($scope){ \
  \   return function(){ \
  \     return $scope; \
  \   }; \
  \ }"
  :: forall e a. Scope a -> Eff (ngrscope :: ReadScopeState a | e) (ScopeState a)

foreign import writeScopeState
  " function writeScopeState(scope){ \
  \   return function($scope){ \
  \     return function(){ \
  \       return angular.extend($scope, scope); \
  \     }; \
  \   }; \
  \ }"
  :: forall e a b. ScopeState b -> Scope a -> Eff (ngwscope :: WriteScopeState a | e) (Scope b)

foreign import readThisState
  " function readThisState($this){ \
  \   return function(){ \
  \     return $this; \
  \   }; \
  \ }"
  :: forall e a. This a -> Eff (ngrthis :: ReadThisState a | e) (ThisState a)

foreign import writeThisState
  " function writeThisState(__this){ \
  \   return function($this){ \
  \     return function(){ \
  \       return angular.extend($this, __this); \
  \     }; \
  \   }; \
  \ }"
  :: forall e a b. ThisState b -> This a -> Eff (ngwthis :: WriteThisState a | e) (This b)

modifyScopeState :: forall e a b
                 .  (ScopeState a -> RWScopeState e a (ScopeState b))
                 -> Scope a
                 -> RWScopeState e a (Scope b)
modifyScopeState k s = do
  s' <- readScopeState s
  w <- k s'
  writeScopeState w s

modifyThisState :: forall e a b
                .  (ThisState a -> RWThisState e a (ThisState b))
                -> This a
                -> RWThisState e a (This b)
modifyThisState k s = do
  s' <- readThisState s
  w <- k s'
  writeThisState w s

foreign import controller
  " function controller(name){ \
  \   return function(module){ \
  \     return function(k){ \
  \       return function(){ \
  \         return module.controller(name, ['$scope', function($scope){ \
  \           return k($scope)(this)(); \
  \         }]); \
  \       }; \
  \     }; \
  \   };\
  \ }"
  :: forall e r a b c d
  .  String
  -> Module
  -> (Scope a -> This b -> ConstructorFnEff e r a b c d)
  -> Eff (ngwmod :: WriteModule | e) Module
