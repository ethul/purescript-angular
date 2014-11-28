module Angular.ST where

import Control.Monad.Eff
import Control.Monad.ST (ST())
import Data.Array.ST (STArray())

foreign import readSTArray "function readSTArray(arr) {\
                           \  return function() {\
                           \    return arr;\
                           \  };\
                           \}" :: forall a h r. STArray h a -> Eff (st :: ST h | r) [a]

foreign import writeSTArray "function writeSTArray(arr1) {\
                            \  return function(arr2){\
                            \    return function(){ \
                            \      arr1.splice(0, arr1.length); \
                            \      for (var i = 0; i < arr2.length; i++) { \
                            \        arr1[i] = arr2[i]; \
                            \      } \
                            \      return arr1;\
                            \    };\
                            \  };\
                            \}" :: forall a h r. STArray h a -> [a] -> Eff (st :: ST h | r) [a]

foreign import pushSTArray "function pushSTArray(arr) {\
                            \  return function(a) {\
                            \    return function() {\
                            \      return arr.push(a);\
                            \    };\
                            \  };\
                            \}" :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Number

foreign import pushAllSTArray "function pushAllSTArray(arr) {\
                              \  return function(as) {\
                              \    return function() {\
                              \      return arr.push.apply(arr, as);\
                              \    };\
                              \  };\
                              \}" :: forall a h r. STArray h a -> [a] -> Eff (st :: ST h | r) Number

foreign import spliceSTArray "function spliceSTArray(arr) {\
                              \  return function(index) {\
                              \    return function(howMany) {\
                              \      return function(bs) {\
                              \        return function() {\
                              \          return arr.splice.apply(arr, [index, howMany].concat(bs));\
                              \        };\
                              \      };\
                              \    };\
                              \  };\
                              \}" :: forall a h r. STArray h a -> Number -> Number -> [a] -> Eff (st :: ST h | r) [a]
