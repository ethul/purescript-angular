module Angular.Interval
  ( Interval()
  , NgInterval()
  , IntervalEff()
  , IntervalPromise()
  , interval
  , interval'
  , interval''
  , intervalk
  , cancel
  ) where

import Control.Monad.Eff
import Data.Function
import Data.Maybe

import Angular.Promise (Promise())

foreign import data Interval :: *

foreign import data NgInterval :: !

type IntervalEff e r = Eff (nginterval :: NgInterval | e) r

type IntervalPromise = Promise Unit Number

foreign import intervalFn
  " function intervalFn(fromMaybe, fn, delay, count, invokeApply, $interval){ \
  \   return function(){ \
  \     return $interval(fn, delay, fromMaybe(undefined)(count), fromMaybe(undefined)(invokeApply)); \
  \   }; \
  \ } "
  :: forall e f r. Fn6 (forall a. a -> Maybe a -> a)
                       (Eff f r)
                       Number
                       (Maybe Number)
                       (Maybe Boolean)
                       Interval
                       (IntervalEff e IntervalPromise)

interval :: forall e f r. Eff f r -> Number -> Number -> Boolean -> Interval -> IntervalEff e IntervalPromise
interval fn delay count invoke = runFn6 intervalFn fromMaybe fn delay (Just count) (Just invoke)

interval' :: forall e f r. Eff f r -> Number -> Number -> Interval -> IntervalEff e IntervalPromise
interval' fn delay count = runFn6 intervalFn fromMaybe fn delay (Just count) Nothing

interval'' :: forall e f r. Eff f r -> Number -> Interval -> IntervalEff e IntervalPromise
interval'' fn delay = runFn6 intervalFn fromMaybe fn delay Nothing Nothing

intervalk :: forall e f r. Number -> Number -> Boolean -> Interval -> Eff f r -> IntervalEff e IntervalPromise
intervalk delay count invoke interval fn = runFn6 intervalFn fromMaybe fn delay (Just count) (Just invoke) interval

foreign import cancelFn
  " function cancelFn(promise, $interval){ \
  \   return function(){ \
  \     return $interval.cancel(promise); \
  \   }; \
  \ } "
  :: forall e. Fn2 IntervalPromise
                   Interval
                   (IntervalEff e Boolean)

cancel :: forall e. IntervalPromise -> Interval -> IntervalEff e Boolean
cancel = runFn2 cancelFn
