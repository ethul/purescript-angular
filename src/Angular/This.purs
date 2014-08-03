module Angular.This
  ( This()
  , Read()
  , Write()
  , ReadWrite()
  , readThis
  , writeThis
  , extendThis
  , modifyThis
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe
import Data.Function

foreign import data This :: # * -> *

foreign import data ReadThis :: !

foreign import data WriteThis :: !

type Read e a = Eff (ngrthis :: ReadThis | e) { | a }

type Write e = Eff (ngwthis :: WriteThis | e) Unit

type ReadWrite e r = Eff (ngrthis :: ReadThis, ngwthis :: WriteThis | e) r

writeThis :: forall e a b. String -> b -> This a -> Write e
writeThis = runFn3 writeThisFn

extendThis :: forall e a b. { | b } -> This a -> Write e
extendThis = runFn2 extendThisFn

modifyThis :: forall e f a b. ({ | a } -> Eff f { | b }) -> This a -> ReadWrite e Unit
modifyThis k t = do
  t' <- readThis t
  w <- unsafeInterleaveEff $ k t'
  extendThis w t

foreign import readThis
  " function readThis($this){ \
  \   return function(){ \
  \     return $this; \
  \   } \
  \ } " :: forall e a. This a -> Read e a

foreign import extendThisFn
  "function extendThisFn(obj, $this){\
  \  return function(){\
  \    angular.extend($this, obj);\
  \  };\
  \}" :: forall e a b. Fn2 { | b } (This a) (Write e)

foreign import writeThisFn
  " function writeThisFn(prop, value, $this){ \
  \   return function(){ \
  \     $this[prop] = value; \
  \   }; \
  \ } " :: forall e a b. Fn3 String b (This a) (Write e)
