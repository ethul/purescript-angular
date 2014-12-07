module TodomvcF.StorageI
  ( Store()
  , storageN
  ) where

import Control.Monad.Eff (Eff())
import Data.Coyoneda (Natural())
import Data.Function (Fn3(..), runFn3)
import Data.Maybe (Maybe(..))

import TodomvcF.StorageF (StorageF(..), Key(), Value())

foreign import data Store :: !

storageN :: forall e. Natural StorageF (Eff (store :: Store | e))
storageN (Get k l) = l <$> runFn3 getFn Nothing Just k
storageN (Set k v a) = const a <$> runFn3 setFn unit k v

foreign import getFn """
  function getFn(nothing, just, key) {
    return function(){
      var value = localStorage.getItem(key);
      return value ? just(value) : nothing;
    };
  }
""" :: forall e. Fn3 (Maybe Value)
                     (Value -> Maybe Value)
                     Key
                     (Eff (store :: Store | e) (Maybe Value))

foreign import setFn """
  function setFn(unit, key, value){
    return function(){
      localStorage.setItem(key, JSON.stringify(value));
      return unit;
    };
  }
""" :: forall e. Fn3 Unit
                     Key
                     Value
                     (Eff (store :: Store | e) Unit)
