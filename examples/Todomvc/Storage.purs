module Todomvc.Storage
  ( Todo(..)
  , Store(..)
  , get
  , put
  ) where

import Control.Monad.Eff

type Todo = { title :: String , completed :: Boolean }

foreign import data Store :: !

storageId :: String
storageId = "todos-angularjs-perf-purs"

foreign import get
  " function get(){ \
  \   return JSON.parse(localStorage.getItem(storageId) || '[]'); \
  \ }"
  :: forall e. Eff (store :: Store | e) [Todo]

foreign import put
  " function put(todos){ \
  \   return function(){ \
  \     return localStorage.setItem(storageId, JSON.stringify(todos)); \
  \   }; \
  \ }"
  :: forall e. [Todo] -> Eff (store :: Store | e) Unit
