module Angular.Log
  ( NgLog()
  , Log()
  , LogEff()
  , log
  , info
  , warn
  , error
  , debug
  ) where

import Control.Monad.Eff
import Data.Function

foreign import data Log :: *

foreign import data NgLog :: !

type LogEff e = Eff (nglog :: NgLog | e) Unit

foreign import logFn
  " function logFn(fn, message, $log){ \
  \   return function(){ \
  \     return $log[fn](expression); \
  \   }; \
  \ } "
  :: forall e a. Fn3 String a Log (LogEff e)

log :: forall e a. a -> Log -> LogEff e
log = runFn3 logFn "log"

info :: forall e a. a -> Log -> LogEff e
info = runFn3 logFn "info"

warn :: forall e a. a -> Log -> LogEff e
warn = runFn3 logFn "warn"

error :: forall e a. a -> Log -> LogEff e
error = runFn3 logFn "error"

debug :: forall e a. a -> Log -> LogEff e
debug = runFn3 logFn "debug"
