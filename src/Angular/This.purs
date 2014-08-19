module Angular.This
  ( This()
  , ReadEff()
  , WriteEff()
  , ReadWriteEff()
  , NgReadThis()
  , NgWriteThis()
  , readThis
  , writeThis
  , extendThis
  , modifyThis
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe
import Data.Function

foreign import data This :: # * -> *

foreign import data NgReadThis :: !

foreign import data NgWriteThis :: !

type ReadEff e a = Eff (ngrthis :: NgReadThis | e) { | a }

type WriteEff e = Eff (ngwthis :: NgWriteThis | e) Unit

type ReadWriteEff e r = Eff (ngrthis :: NgReadThis, ngwthis :: NgWriteThis | e) r

writeThis :: forall e a b. String -> b -> This a -> WriteEff e
writeThis = runFn3 writeThisFn

extendThis :: forall e a b. { | b } -> This a -> WriteEff e
extendThis = runFn2 extendThisFn

modifyThis :: forall e f a b. ({ | a } -> Eff f { | b }) -> This a -> ReadWriteEff e Unit
modifyThis k t = do
  t' <- readThis t
  w <- unsafeInterleaveEff $ k t'
  extendThis w t

foreign import readThis
  " function readThis($this){ \
  \   return function(){ \
  \     return $this; \
  \   } \
  \ } " :: forall e a. This a -> ReadEff e a

foreign import extendThisFn
  "function extendThisFn(obj, $this){\
  \  return function(){\
  \    angular.extend($this, obj);\
  \  };\
  \}" :: forall e a b. Fn2 { | b } (This a) (WriteEff e)

foreign import writeThisFn
  " function writeThisFn(prop, value, $this){ \
  \   return function(){ \
  \     $this[prop] = value; \
  \   }; \
  \ } " :: forall e a b. Fn3 String b (This a) (WriteEff e)
