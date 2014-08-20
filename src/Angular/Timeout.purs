module Angular.Timeout
  ( Timeout()
  , NgTimeout()
  , TimeoutEff()
  , TimeoutPromise()
  , timeout
  , timeout'
  , timeout''
  , timeoutk
  , cancel
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Exception (Error())
import Data.Function
import Data.Maybe

import Angular.Promise (Promise())

foreign import data Timeout :: *

foreign import data NgTimeout :: !

type TimeoutEff e r = Eff (ngtimeout :: NgTimeout | e) r

type TimeoutPromise a = Promise Error a

foreign import timeoutFn
  " function timeoutFn(fromMaybe, fn, delay, invokeApply, $timeout){ \
  \   return function(){ \
  \     return $timeout(fn, fromMaybe(undefined)(delay), fromMaybe(undefined)(invokeApply)); \
  \   }; \
  \ } "
  :: forall e f r. Fn5 (forall a. a -> Maybe a -> a)
                       (Eff f r)
                       (Maybe Number)
                       (Maybe Boolean)
                       Timeout
                       (TimeoutEff e (TimeoutPromise r))

timeout :: forall e f r. Eff f r -> Number -> Boolean -> Timeout -> TimeoutEff e (TimeoutPromise r)
timeout fn delay invoke = runFn5 timeoutFn fromMaybe fn (Just delay) (Just invoke)

timeout' :: forall e f r. Eff f r -> Number -> Timeout -> TimeoutEff e (TimeoutPromise r)
timeout' fn delay = runFn5 timeoutFn fromMaybe fn (Just delay) Nothing

timeout'' :: forall e f r. Eff f r -> Timeout -> TimeoutEff e (TimeoutPromise r)
timeout'' fn = runFn5 timeoutFn fromMaybe fn Nothing Nothing

timeoutk :: forall e f r. Number -> Boolean -> Timeout -> Eff f r -> TimeoutEff e (TimeoutPromise r)
timeoutk delay invoke timeout fn = runFn5 timeoutFn fromMaybe fn (Just delay) (Just invoke) timeout

foreign import cancelFn
  " function cancelFn(fromMaybe, promise, $timeout){ \
  \   return function(){ \
  \     return $timeout.cancel(fromMaybe(undefined)(promise)); \
  \   }; \
  \ } "
  :: forall e r. Fn3 ((TimeoutPromise r) -> Maybe (TimeoutPromise r) -> (TimeoutPromise r))
                     (Maybe (TimeoutPromise r))
                     Timeout
                     (TimeoutEff e Boolean)

cancel :: forall e r. TimeoutPromise r -> Timeout -> TimeoutEff e Boolean
cancel promise = runFn3 cancelFn fromMaybe (Just promise)
