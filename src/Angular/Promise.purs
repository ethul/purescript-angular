module Angular.Promise
  ( Promise()
  , then1
  , then1'
  , then2
  , then2'
  , then3
  , then3'
  , catch
  , finally
  ) where

import Control.Monad.Eff
import Data.Function

foreign import data Promise :: * -> * -> *

foreign import then1Fn
  """
  function then1Fn(f, fa){
    return fa['then'](f);
  }
  """ :: forall a b c d. Fn2 (b -> c) (Promise a b) (Promise a d)

then1 :: forall a b c. (b -> Promise a c) -> Promise a b -> Promise a c
then1 = runFn2 then1Fn

then1' :: forall a b c. (b -> c) -> Promise a b -> Promise a c
then1' = runFn2 then1Fn

foreign import then2Fn
  """
  function then2Fn(f, g, fa){
    return fa['then'](f, g);
  }
  """ :: forall s t a b c d. Fn3 (b -> d) (a -> c) (Promise a b) (Promise s t)

then2 :: forall a b c d. (b -> Promise c d) -> (a -> Promise c d) -> Promise a b -> Promise c d
then2 = runFn3 then2Fn

then2' :: forall a b c d. (b -> d) -> (a -> c) -> Promise a b -> Promise c d
then2' = runFn3 then2Fn

foreign import then3Fn
  """
  function then3Fn(f, g, h, fa){
    return fa['then'](f, g, function(a){
      return h(a)();
    });
  }
  """ :: forall e q r s t a b c d. Fn4 (b -> d)
                                       (a -> c)
                                       (s -> Eff e t)
                                       (Promise a b)
                                       (Promise q r)

then3 :: forall e s t a b c d. (b -> Promise c d) -> (a -> Promise c d) -> (s -> Eff e t) -> Promise a b -> Promise c d
then3 = runFn4 then3Fn

then3' :: forall e s t a b c d. (b -> d) -> (a -> c) -> (s -> Eff e t) -> Promise a b -> Promise c d
then3' = runFn4 then3Fn

foreign import catchFn
  """
  function catchFn(f, fa){ \
    return fa['catch'](f); \
  }
  """ :: forall a b c d. Fn2 (a -> Promise c d) (Promise a b) (Promise c d)

catch :: forall a b c d. (a -> Promise c d) -> Promise a b -> Promise c d
catch = runFn2 catchFn

foreign import finallyFn
  """
  function finallyFn(f, fa){
    return fa['finally'](f);
  }
  """ :: forall e r a b. Fn2 (Eff e r) (Promise a b) (Promise a b)

finally :: forall e r a b. Eff e r -> Promise a b -> Promise a b
finally = runFn2 finallyFn
