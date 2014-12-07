module TodomvcF.ThisI
  ( FThis()
  , thisN
  ) where

import Control.Monad.Eff (Eff())
import Data.Coyoneda (Natural())
import Data.Function (Fn3(..), runFn3)

import TodomvcF.ThisF (ThisF(..), ForeignThis())

foreign import data FThis :: !

thisN :: forall b e. Natural (ThisF b) (Eff (this :: FThis | e))
thisN (Read t k) = (pure <<< k <<< readFn) t
thisN (Extend b t a) = const a <$> runFn3 extendFn unit b t

foreign import readFn """
  function readFn(that) {
    return that;
  }
""" :: forall b. ForeignThis b -> b

foreign import extendFn """
  function extendFn(unit, b, that){
    return function(){
      angular.extend(that, b);
      return unit;
    };
  }
""" :: forall b e. Fn3 Unit
                       b
                       (ForeignThis b)
                       (Eff (this :: FThis | e) Unit)
