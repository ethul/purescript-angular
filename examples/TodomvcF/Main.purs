module TodomvcF.Main where

import Control.Monad.Eff
import Data.Either (either)

import Angular.Module (controller, ngmodule')
import Angular.This (extendThis)

import TodomvcF.ThisF (ForeignThis())
import TodomvcF.TodomvcF (add)
import TodomvcF.TodomvcI (AppT(), run)

mainctrl this = extendThis { newTodo: ""
                           , todos: []
                           , add: add
                           , run: (run <<< toForeignThis) this } this

main = do
  m <- ngmodule' "todomvcf" []
  controller "Main" mainctrl' m

foreign import toForeignThis """
  function toForeignThis(that){
    return that;
  }
""" :: forall a. a -> ForeignThis AppT

foreign import mainctrl' """
  /*@ngInject*/function mainctrl$prime() {
    var impl = mainctrl(this);
    return impl.apply(this, []);
  }
""" :: forall e a. Eff e Unit
