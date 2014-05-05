module Angular.Module where

import Control.Monad.Eff

import Angular.Injector (InjectDependency(..))

foreign import data Module :: *

foreign import data ReadModule :: !

foreign import data WriteModule :: !

foreign import data RegisterModule :: !

foreign import newModule
  " function newModule(name){ \
  \   return function(requires){ \
  \     return function(){ \
  \       return angular.module(name, requires); \
  \     }; \
  \   }; \
  \ } "
  :: forall e. String -> [String] -> Eff (ngwmod :: WriteModule | e) Module

foreign import getModule
  " function getModule(name){ \
  \   return function(){ \
  \     return angular.module(name); \
  \   }; \
  \ } "
  :: forall e. String -> Eff (ngrmod :: ReadModule | e) Module


-- | directive in Angular.Directive

-- | controller in Angular.Controller

foreign import factory
  " function factory(name){ \
  \   return function(k){ \
  \     return function(module){ \
  \       return function(){ \
  \         return module.factory(name, k); \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a
  .  String
  -> Eff (nginj :: InjectDependency | e) { | a }
  -> Module
  -> Eff (nggmod :: RegisterModule | e) Module

foreign import service
  " function service(name){ \
  \   return function(k){ \
  \     return function(module){ \
  \       return function(){ \
  \         return module.service(name, function(){ \
  \           return angular.extend(this, k()); \
  \         }); \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a
  .  String
  -> Eff (nginj :: InjectDependency | e) { | a }
  -> Module
  -> Eff (nggmod :: RegisterModule | e) Module

foreign import constant
  " function constant(name){ \
  \   return function(c){ \
  \     return function(module){ \
  \       return function(){ \
  \         return module.constant(name, c); \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a
  .  String
  -> a
  -> Module
  -> Eff (nggmod :: RegisterModule | e) Module

foreign import value
  " function value(name){ \
  \   return function(v){ \
  \     return function(module){ \
  \       return function(){ \
  \         return module.value(name, v); \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a
  .  String
  -> a
  -> Module
  -> Eff (nggmod :: RegisterModule | e) Module

foreign import provider
  " function provider(name){ \
  \   return function(k){ \
  \     return function(module){ \
  \       return function(){ \
  \         return module.service(name, function(){ \
  \           return angular.extend(this, k()); \
  \         }); \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a b
  .  String
  -> Eff (nginj :: InjectDependency | e) { "$get" :: Eff (nginj :: InjectDependency | e) a | b }
  -> Module
  -> Eff (nggmod :: RegisterModule | e) Module

-- | decorator
